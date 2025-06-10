
#!/bin/bash

set -e  # this is for exiting script if error occured

echo -e ".....................................\n"

echo -e "1. Building for local devlopment...\n"
npm run build

echo -e "2. Cleaning destination directory...\n"
rm -rf /usr/share/nginx/editnow.site/html/* 

echo -e "3. Copying new files to server...\n"
cp -r dist/* /usr/share/nginx/editnow.site/html/

echo -e "4. Reloading Nginx...\n"
sudo systemctl reload nginx

echo -e "5. Local build complete\n"

echo -e "......................................\n"


