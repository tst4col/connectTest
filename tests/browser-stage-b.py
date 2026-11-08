#!/usr/bin/env python3
"""Browser journey check for connectTest Stage B.

The app must already be running on a loopback URL with the owner gate
enabled, e.g. (synthetic password; never a real credential):

  CONNECTTEST_UPLOAD_ENABLED=true \
  CONNECTTEST_UPLOAD_PASSWORD=<synthetic-test-password> \
  .rvenv/bin/Rscript -e 'shiny::runApp(".", host="127.0.0.1", port=7869)'

Run:
  /tmp/opencode/pwvenv/bin/python tests/browser-stage-b.py http://127.0.0.1:7869

The script types the password from CONNECTTEST_UPLOAD_PASSWORD into the
upload form; the password value itself is never printed. Journey:
  gate -> wrong password rejected -> correct password accepted ->
  unauthenticated upload rejected (before any import) ->
  create file locally via the app -> upload it -> edit -> snapshot ->
  download -> re-upload across sessions -> distinct per-session values.
"""

import os
import sys
import tempfile
from pathlib import Path

from playwright.sync_api import sync_playwright

BASE_URL = sys.argv[1].rstrip("/") if len(sys.argv) > 1 else "http://127.0.0.1:7869"
PASSWORD = os.environ.get("CONNECTTEST_UPLOAD_PASSWORD", "")
OUTPUT_DIR = Path(tempfile.mkdtemp(prefix="connectTest-browser-b-"))

RESULTS = []


def pass_step(text: str) -> None:
    RESULTS.append(f"PASS: {text}")


def wait_for_notification(page, text: str) -> None:
    page.locator(".shiny-notification", has_text=text).first.wait_for(timeout=15000)


def edit_and_save(page, title: str, instructions: str, servings: int, setting: str) -> None:
    page.fill("#recipeTitle", title)
    page.fill("#recipeInstructions", instructions)
    page.fill("#recipeServings", str(servings))
    page.click("#saveRecipe")
    wait_for_notification(page, "Rezept wurde in der Arbeitsdatenbank gespeichert")
    page.fill("#collectionName", setting)
    page.click("#saveCollectionSetting")
    wait_for_notification(page, "Sammlungseinstellung wurde in der Arbeitsdatenbank gespeichert")
    page.locator("#recipePreview").get_by_text(title, exact=True).wait_for(timeout=15000)


def make_snapshot(page) -> None:
    page.click("#makeSnapshot")
    page.locator("#downloadDatabase").wait_for(state="visible", timeout=15000)


def download_snapshot(page, path: Path) -> Path:
    with page.expect_download(timeout=15000) as download_info:
        page.click("#downloadDatabase")
    download = download_info.value
    download.save_as(path)
    if not path.exists() or path.stat().st_size == 0:
        raise AssertionError("Downloaded database is missing or empty")
    return path


def upload_file(page, path: Path) -> None:
    page.set_input_files("#uploadDatabaseFile", str(path))


def import_completes(page, title_fragment: str) -> None:
    wait_for_notification(page, "Der Import ist abgeschlossen")
    page.locator("#recipePreview").get_by_text(title_fragment, exact=False).first.wait_for(
        timeout=15000
    )


def main() -> int:
    if not PASSWORD:
        print("FAIL: CONNECTTEST_UPLOAD_PASSWORD is not set in the environment")
        return 1

    with sync_playwright() as playwright:
        browser = playwright.chromium.launch()
        owner = browser.new_context(accept_downloads=True)
        other = browser.new_context(accept_downloads=True)
        page_one = owner.new_page()
        page_two = other.new_page()
        errors = []
        for page in (page_one, page_two):
            page.on("pageerror", lambda error: errors.append(str(error)))

        for page in (page_one, page_two):
            page.goto(BASE_URL, wait_until="networkidle")
            page.locator("#recipeTitle").wait_for(timeout=20000)

        # --- reactive status: locked -> unlocked (M1 regression) -------
        page_one.locator("#uploadGateStatus").get_by_text(
            "bitte Passwort eingeben", exact=False
        ).first.wait_for(timeout=15000)
        # --- wrong password rejected at the gate ----------------------
        page_one.fill("#ownerPassword", "definitiv-falsch")
        page_one.click("#submitOwnerPassword")
        wait_for_notification(page_one, "Passwort nicht akzeptiert")
        page_one.wait_for_timeout(500)
        # the failed attempt must not flip the displayed status
        page_one.locator("#uploadGateStatus").get_by_text(
            "bitte Passwort eingeben", exact=False
        ).first.wait_for(timeout=15000)
        page_one.fill("#ownerPassword", PASSWORD)
        page_one.click("#submitOwnerPassword")
        wait_for_notification(page_one, "Upload ist für diese Sitzung freigeschaltet")
        # authentication invalidates the gate status output (M1): it re-renders
        page_one.locator("#uploadGateStatus").get_by_text(
            "Upload für diese Sitzung freigeschaltet", exact=False
        ).first.wait_for(timeout=15000)
        pass_step("gate accepts the correct synthetic password after rejecting a wrong one (status reacts)")

        # --- local creation via the existing services -----------------
        edit_and_save(
            page_one,
            "Browserisch Erstes Rezept – Größe geprüft",
            "Straße 3: Zubereitung mit ÄÖÜ und ß.",
            4,
            "Browser-Sammlung Eins – Sonntagsküche",
        )
        edit_and_save(
            page_two,
            "Browserisch Zweites Rezept",
            "Andere Sitzung, andere Küche.",
            8,
            "Browser-Sammlung Zwei – Straße 2",
        )
        if page_one.input_value("#recipeTitle") == page_two.input_value("#recipeTitle"):
            raise AssertionError("Browser sessions unexpectedly share recipe state")
        pass_step("two browser contexts show distinct edited values")

        # --- create the file locally through the app itself -----------
        make_snapshot(page_one)
        file_created_locally = download_snapshot(page_one, OUTPUT_DIR / "created-locally.duckdb")
        pass_step("generic file created locally through the app snapshot download")

        # --- unauthenticated upload is rejected before any import ------
        upload_file(page_two, file_created_locally)
        wait_for_notification(page_two, "Besitzerfreigabe erforderlich")
        page_two.wait_for_timeout(500)
        # context two's content must be its own edited values, unchanged
        page_two.locator("#recipePreview").get_by_text(
            "Browserisch Zweites Rezept", exact=True
        ).wait_for(timeout=15000)
        if page_two.locator("text=Import abgeschlossen").count() != 0:
            raise AssertionError("Unauthenticated session achieved an import")
        pass_step("unauthenticated upload is rejected without an import")

        # --- authenticate in context two and re-upload the same file ---
        page_two.fill("#ownerPassword", "erstfalsch")
        page_two.click("#submitOwnerPassword")
        wait_for_notification(page_two, "Passwort nicht akzeptiert")
        page_one.wait_for_timeout(500)
        page_two.fill("#ownerPassword", PASSWORD)
        page_two.click("#submitOwnerPassword")
        wait_for_notification(page_two, "Upload ist für diese Sitzung freigeschaltet")
        upload_file(page_two, file_created_locally)
        import_completes(page_two, "Browserisch Erstes Rezept")
        pass_step("re-uploading the locally created file imports its values")

        # --- edit, snapshot and download in each session ---------------
        edit_and_save(
            page_two,
            "Browserisch Zweites Importiert und geändert",
            "Nach dem Import weiter bearbeitet.",
            6,
            "Browser-Sammlung Zwei nach Import",
        )
        make_snapshot(page_two)
        file_two = download_snapshot(page_two, OUTPUT_DIR / "browser-two.duckdb")

        make_snapshot(page_one)
        file_one = download_snapshot(page_one, OUTPUT_DIR / "browser-one.duckdb")
        if file_one.read_bytes() == file_two.read_bytes():
            raise AssertionError("Two browser sessions produced identical database downloads")
        pass_step("two browser sessions produced distinct downloadable databases")

        # --- cross-session round trip: upload session two's file into session one
        edit_and_save(
            page_one,
            "Browserisch Erstes nach Download",
            "Sitzung eins fährt fort.",
            5,
            "Browser-Sammlung Eins fortgesetzt",
        )
        upload_file(page_one, file_two)
        import_completes(page_one, "Browserisch Zweites Importiert und geändert")
        # session one now displays session two's imported content (round trip),
        # while the sessions remain separate installations with their own state
        pass_step("cross-session upload imported session two's values into session one")

        # a fresh edit in session one diverges again from session two's file
        edit_and_save(
            page_one,
            "Browserisch Erstes nach Rundreise",
            "Sitzung eins ist eigenständig geblieben.",
            3,
            "Browser-Sammlung Eins nach Rundreise",
        )
        make_snapshot(page_one)
        file_one_after = download_snapshot(page_one, OUTPUT_DIR / "browser-one-after.duckdb")
        if file_one_after.read_bytes() == file_two.read_bytes():
            raise AssertionError("Post-round-trip download unexpectedly equals session two's file")
        pass_step("sessions stay independent across the cross-session round trip")

        owner.close()
        other.close()
        browser.close()

    if errors:
        for error in errors:
            print(f"FAIL: browser page error: {error}")
        return 1
    for line in RESULTS:
        print(line)
    print(f"Downloaded files: {OUTPUT_DIR}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
