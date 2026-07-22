// Render a cassette to a GIF (and MP4) by stepping player.html on a virtual
// clock in headless Chromium, one screenshot per frame, then ffmpeg.
//
//   node capture.mjs <name> [--fps 12] [--width 760] [--probe t1,t2,...]
//
// Frames → frames/<name>/, output → gifs/<name>.{gif,mp4}. --probe writes a few
// timestamped PNGs for inspection instead of a full render.

import { chromium } from "playwright";
import { createServer } from "node:http";
import { spawn } from "node:child_process";
import { readFile, mkdir, rm, readdir } from "node:fs/promises";
import { extname, join } from "node:path";

const ROOT = process.cwd();
const args = process.argv.slice(2);
const name = args[0] || "orion";
const opt = (k, d) => { const i = args.indexOf("--" + k); return i >= 0 ? args[i + 1] : d; };
const fps = +opt("fps", 12);
const width = +opt("width", 760);
const speed = +opt("speed", 1);      // >1 compresses playback (fewer frames)
const probe = opt("probe", null);

const MIME = { ".html":"text/html", ".json":"application/json", ".js":"text/javascript",
               ".css":"text/css", ".png":"image/png" };
const server = createServer(async (req, res) => {
  try {
    const p = join(ROOT, decodeURIComponent(req.url.split("?")[0]));
    const body = await readFile(p);
    res.writeHead(200, { "content-type": MIME[extname(p)] || "application/octet-stream" });
    res.end(body);
  } catch { res.writeHead(404); res.end("not found"); }
});
await new Promise(r => server.listen(0, r));
const port = server.address().port;
const base = `http://localhost:${port}`;

const run = (cmd, a) => new Promise((res, rej) => {
  const p = spawn(cmd, a, { stdio: "inherit" });
  p.on("close", c => c === 0 ? res() : rej(new Error(`${cmd} exited ${c}`)));
});

const browser = await chromium.launch();
const page = await browser.newPage({ viewport: { width: 980, height: 760 }, deviceScaleFactor: 2 });
await page.goto(`${base}/player.html?c=${name}&clean=1`, { waitUntil: "networkidle" });
await page.waitForFunction(() => window.PLAYER && window.PLAYER.ready && window.PLAYER.duration > 0);
const dur = await page.evaluate(() => window.PLAYER.duration);
const film = page.locator("#film");
console.log(`${name}: ${(dur/1000).toFixed(1)}s`);

if (probe) {
  await mkdir("probe", { recursive: true });
  for (const t of probe.split(",").map(Number)) {
    await page.evaluate(ms => window.PLAYER.seek(ms), t);
    await page.waitForTimeout(30);
    await film.screenshot({ path: `probe/${name}-${t}.png` });
    console.log("probe", t);
  }
} else {
  const dir = `frames/${name}`;
  await rm(dir, { recursive: true, force: true });
  await mkdir(dir, { recursive: true });
  const N = Math.ceil((dur / speed / 1000) * fps);
  for (let i = 0; i <= N; i++) {
    const t = Math.min(dur, (i / fps) * 1000 * speed);
    await page.evaluate(ms => window.PLAYER.seek(ms), t);
    await page.waitForTimeout(12);
    await film.screenshot({ path: `${dir}/f-${String(i).padStart(4, "0")}.png` });
  }
  console.log(`${N + 1} frames`);
  await mkdir("gifs", { recursive: true });
  // scale to even dims (-2) — h264 needs even height. Flat palette, no dither.
  const vf = `fps=${fps},scale=${width}:-2:flags=lanczos`;
  await run("ffmpeg", ["-y", "-framerate", String(fps), "-i", `${dir}/f-%04d.png`,
    "-vf", `${vf},split[s0][s1];[s0]palettegen=max_colors=64:stats_mode=full[p];[s1][p]paletteuse=dither=none`,
    `gifs/${name}.gif`]);
  await run("ffmpeg", ["-y", "-framerate", String(fps), "-i", `${dir}/f-%04d.png`,
    "-vf", `${vf},format=yuv420p`, "-movflags", "+faststart", `gifs/${name}.mp4`]);
  const sz = async f => (await readFile(f)).length;
  console.log(`gifs/${name}.gif  ${((await sz(`gifs/${name}.gif`))/1e6).toFixed(1)} MB`);
  console.log(`gifs/${name}.mp4  ${((await sz(`gifs/${name}.mp4`))/1e6).toFixed(1)} MB`);
}

await browser.close();
server.close();
