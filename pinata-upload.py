import os
import json
import requests

PINATA_API_KEY = os.environ["PINATA_API_KEY"]
PINATA_SECRET_API_KEY = os.environ["PINATA_SECRET_API_KEY"]
VERSION=json.load(open("package.json"))["version"]

files = [
        ("file", (f"{VERSION}/cactus.js",     open('dist/cactus.js'),     'text/javascript')),
        ("file", (f"{VERSION}/cactus.js.map", open('dist/cactus.js.map'), 'application/json')),
        ("file", (f"{VERSION}/style.css",     open('dist/style.css'),     'text/css')),
        ("file", (f"{VERSION}/style.css.map", open('dist/style.js.map'),  'application/json'))
]

print(f"Uploading {VERSION}: {len(files)} files...")
r = requests.post(
      "https://api.pinata.cloud/pinning/pinFileToIPFS",
      headers={
          "pinata_api_key": PINATA_API_KEY,
          "pinata_secret_api_key": PINATA_SECRET_API_KEY
      },
      files=files
)

if r.status_code == 200:
    print("Success!")
else:
    print(f"Error: {r.status_code}")

print(r.json())
