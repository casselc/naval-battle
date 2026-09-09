const {defineConfig, devices} = require("@playwright/test");

const port = Number(process.env.NAVAL_DEMO_PORT || 18420);

module.exports = defineConfig({
  testDir: "test/browser",
  timeout: 60_000,
  expect: {timeout: 12_000},
  fullyParallel: false,
  workers: 1,
  reporter: [["list"]],
  outputDir: "target/telemetry/playwright",
  use: {
    ...devices["Desktop Chrome"],
    baseURL: `http://127.0.0.1:${port}`,
    viewport: {width: 1280, height: 900},
    colorScheme: "dark",
    reducedMotion: "reduce",
    trace: "retain-on-failure",
    screenshot: "only-on-failure",
    video: {mode: "on", size: {width: 1280, height: 900}},
  },
  projects: [{name: "demo", testMatch: /telemetry-tour\.spec\.js/}],
});
