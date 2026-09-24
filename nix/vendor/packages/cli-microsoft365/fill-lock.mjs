// Usage: node fill-lock.mjs <path to cli-microsoft365 checkout>
// Writes ./package-lock.json: a copy of the checkout's npm-shrinkwrap.json with the
// `resolved`/`integrity` fields npm sometimes omits filled in from the
// registry, so Nix can prefetch every dependency. Versions are unchanged.
import fs from 'fs';
import path from 'path';
import url from 'url';

const here = path.dirname(url.fileURLToPath(import.meta.url));
const root = process.argv[2];
const lock = JSON.parse(fs.readFileSync(path.join(root, 'npm-shrinkwrap.json'), 'utf8'));

const missing = Object.entries(lock.packages)
  .filter(([key, pkg]) => key !== '' && !pkg.link && !pkg.resolved);

const nameOf = (key, pkg) => pkg.name ?? key.slice(key.lastIndexOf('node_modules/') + 'node_modules/'.length);

for (let i = 0; i < missing.length; i += 32) {
  await Promise.all(missing.slice(i, i + 32).map(async ([key, pkg]) => {
    const name = nameOf(key, pkg);
    const res = await fetch(`https://registry.npmjs.org/${name.replace('/', '%2F')}/${pkg.version}`);
    if (!res.ok) {
      throw new Error(`${name}@${pkg.version}: ${res.status}`);
    }
    const { dist } = await res.json();
    pkg.resolved = dist.tarball;
    pkg.integrity = dist.integrity;
  }));
}

fs.writeFileSync(path.join(here, 'package-lock.json'), JSON.stringify(lock, null, 2) + '\n');
console.log(`Filled ${missing.length} entries`);
