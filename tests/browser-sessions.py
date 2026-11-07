#!/usr/bin/env python3
"""Local browser-session check for connectTest Stage A.

The app must already be running on a loopback URL. Example:
  .rvenv/bin/Rscript -e 'shiny::runApp(".", host="127.0.0.1", port=7867)'

This script uses Playwright. It is optional test tooling, not an application
or deployment dependency. Install it in a user-level venv if needed:
  python3 -m venv /tmp/opencode/pwvenv
  /tmp/opencode/pwvenv/bin/pip install playwright
  /tmp/opencode/pwvenv/bin/playwright install chromium

Run:
  /tmp/opencode/pwvenv/bin/python tests/browser-sessions.py http://127.0.0.1:7867
"""

import hashlib
import sys
import tempfile
from pathlib import Path

from playwright.sync_api import sync_playwright

BASE_URL = sys.argv[1].rstrip("/") if len(sys.argv) > 1 else "http://127.0.0.1:7867"
OUTPUT_DIR = Path(tempfile.mkdtemp(prefix="connectTest-browser-"))


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def wait_for_notification(page, text: str) -> None:
    page.locator(".shiny-notification", has_text=text).wait_for(timeout=10000)


def edit_and_save(page, title: str, instructions: str, servings: int, setting: str) -> None:
    page.fill("#recipeTitle", title)
    page.fill("#recipeInstructions", instructions)
    page.fill("#recipeServings", str(servings))
    page.click("#saveRecipe")
    wait_for_notification(page, "Rezept wurde in der Arbeitsdatenbank gespeichert")
    page.fill("#collectionName", setting)
    page.click("#saveCollectionSetting")
    wait_for_notification(page, "Sammlungseinstellung wurde in der Arbeitsdatenbank gespeichert")
    page.locator("#recipePreview").get_by_text(title, exact=True).wait_for(timeout=10000)
    page.locator("#settingPreview").get_by_text(setting, exact=True).wait_for(timeout=10000)


def make_snapshot(page) -> None:
    page.click("#makeSnapshot")
    page.locator("#downloadDatabase").wait_for(state="visible", timeout=15000)


def download_snapshot(page, path: Path) -> None:
    with page.expect_download(timeout=15000) as download_info:
        page.click("#downloadDatabase")
    download = download_info.value
    download.save_as(path)
    if not path.exists() or path.stat().st_size == 0:
        raise AssertionError("Downloaded database is missing or empty")


def main() -> int:
    results = []
    with sync_playwright() as playwright:
        browser = playwright.chromium.launch()
        first = browser.new_context(accept_downloads=True)
        second = browser.new_context(accept_downloads=True)
        page_one = first.new_page()
        page_two = second.new_page()
        errors = []
        for page in (page_one, page_two):
            page.on("pageerror", lambda error: errors.append(str(error)))

        page_one.goto(BASE_URL, wait_until="networkidle")
        page_two.goto(BASE_URL, wait_until="networkidle")
        page_one.locator("#recipeTitle").wait_for(timeout=15000)
        page_two.locator("#recipeTitle").wait_for(timeout=15000)
        page_one.get_by_text("Status: Noch keine Momentaufnahme", exact=False).wait_for(
            timeout=10000
        )

        edit_and_save(
            page_one,
            "Browser-Eins Käsespätzle",
            "Größe der Portion prüfen und die Soße abschmecken.",
            3,
            "Browser-Sammlung Eins – Straße 1",
        )
        edit_and_save(
            page_two,
            "Browser-Zwei Kartoffelsuppe",
            "Für die zweite Sitzung würfeln und köcheln.",
            7,
            "Browser-Sammlung Zwei – Straße 2",
        )

        if page_one.input_value("#recipeTitle") == page_two.input_value("#recipeTitle"):
            raise AssertionError("Browser sessions unexpectedly share recipe state")
        results.append("PASS: two browser contexts show distinct edited values")

        make_snapshot(page_one)
        make_snapshot(page_two)
        page_one.get_by_text(
            "Status: Keine Änderungen seit der letzten Momentaufnahme", exact=False
        ).wait_for(timeout=10000)

        first_download = OUTPUT_DIR / "browser-one.duckdb"
        second_download = OUTPUT_DIR / "browser-two.duckdb"
        download_snapshot(page_one, first_download)
        download_snapshot(page_two, second_download)

        if sha256(first_download) == sha256(second_download):
            raise AssertionError("Two browser sessions produced identical database downloads")
        results.append("PASS: two browser sessions produced distinct downloadable databases")

        # A download must not prove a durable save or clear the dirty state.
        page_one.fill("#recipeTitle", "Browser-Eins nach Download")
        page_one.click("#saveRecipe")
        wait_for_notification(page_one, "Rezept wurde in der Arbeitsdatenbank gespeichert")
        page_one.get_by_text("Änderungen seit der letzten Momentaufnahme", exact=False).wait_for(
            timeout=10000
        )
        stale_download = OUTPUT_DIR / "browser-one-stale.duckdb"
        download_snapshot(page_one, stale_download)
        page_one.get_by_text("Änderungen seit der letzten Momentaufnahme", exact=False).wait_for(
            timeout=10000
        )
        results.append("PASS: download keeps the dirty indicator and marks the offered snapshot as older")

        # Reloading creates a fresh temporary session with default content.
        page_one.reload(wait_until="networkidle")
        page_one.locator("#recipeTitle").wait_for(timeout=15000)
        default_title = page_one.input_value("#recipeTitle")
        if default_title != "Käsespätzle mit Röstzwiebeln":
            raise AssertionError(f"Reload did not create a fresh default session: {default_title}")
        if "Browser-Eins" in default_title:
            raise AssertionError("Reload retained previous temporary session content")
        results.append("PASS: browser reload starts a fresh temporary session")

        first.close()
        second.close()
        browser.close()

    if errors:
        for error in errors:
            print(f"FAIL: browser page error: {error}")
        return 1
    for line in results:
        print(line)
    print(f"Downloaded files: {OUTPUT_DIR}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
