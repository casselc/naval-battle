const {test, expect} = require("@playwright/test");
const path = require("node:path");

const service = "naval-battle-demo";
const media = (...parts) => path.join("docs", "demo", ...parts);
const pause = (milliseconds) => new Promise((resolve) => setTimeout(resolve, milliseconds));
const screenshot = (page, name) => page.screenshot({path: media(name), fullPage: true});

test("tour real woven game telemetry from trace to editable chart", async ({page}) => {
  // The browser starts as soon as the viewer is healthy. Give the in-process
  // batch processors one bounded retry window to publish the first game data.
  await expect.poll(async () => {
    await page.goto("/oscope/telemetry");
    return page.getByLabel("Service").locator(`option[value="${service}"]`).count();
  }).toBe(1);

  await page.getByLabel("Service").selectOption(service);
  await page.getByLabel("Operation or name").fill("game.action.fire");
  await page.getByRole("button", {name: "Apply filters"}).click();
  const trace = page.locator(".otel-trace-list > li").first();
  await expect(trace).toContainText("game.action.fire");
  await trace.getByRole("link").click();
  const dialog = page.getByRole("dialog", {name: "Trace detail"});
  await expect(dialog).toContainText("game.action.fire");
  await expect(dialog).toContainText("fired");
  await dialog.screenshot({path: media("02-fire-trace.png")});
  await pause(900);

  await page.keyboard.press("Escape");
  await page.goto(`/oscope/events?signal=metrics&metric-kind=sum&service=${service}`
                  + "&search=io.github.casselc.game_engine.frames&window=15m&limit=10");
  const events = page.locator("#oscope-events");
  await expect(events).toContainText("io.github.casselc.game_engine.frames");
  await screenshot(page, "03-frame-metric.png");
  await pause(900);

  await page.goto("/oscope?signal=metrics&field=metric-unit&window=15m&limit=6");
  await expect(page.locator("#oscope-screen svg")).toBeVisible();
  await expect(page.locator("#oscope-screen")).toContainText("Metric Unit");
  await expect(page.locator("#oscope-screen")).toContainText("{action}");
  await expect(page.locator("#oscope-screen")).toContainText("{frame}");
  await screenshot(page, "04-metric-chart.png");
  await pause(900);

  await page.getByRole("link", {name: "Edit this chart"}).click();
  const editor = page.getByLabel("Chart specification");
  await expect(editor).toHaveValue(/:source :current-query/);
  await expect(editor).toHaveValue(/:mark :bar/);
  const actualSpec = await editor.inputValue();
  await editor.fill(actualSpec
    .replace(/:title "[^"]+"/, ':title "Naval battle metric units"')
    .replace(/:palette \[[^\]]+\]/,
             ':palette ["#0ea5e9" "#f97316"]'));
  await expect(page.locator("#plotje-preview svg")).toBeVisible();
  await expect(page.locator("#plotje-preview"))
    .toContainText("Naval battle metric units");
  await expect(editor).toHaveValue(/:source :current-query/);
  await expect(editor).toHaveValue(/:mark :bar/);
  await expect(editor).toHaveValue(/#0ea5e9/);
  await screenshot(page, "05-plotje-query-edit.png");
  await pause(1200);

  const video = page.video();
  await page.close();
  await video.saveAs(media("naval-telemetry-tour.webm"));
});
