#!/bin/bash

set -e  

echo -e ".....................................\n"

echo -e "1. Building project...\n"
npm run build

echo -e "2. Cleaning destination directory...\n"
ssh ec2 'rm -rf /var/www/editnow.site/html/*'

echo -e "3. Copying new files to server...\n"
scp -r dist/* ec2:/var/www/editnow.site/html/

echo -e "4. Reloading Nginx...\n"
ssh ec2 'sudo systemctl reload nginx'

echo -e "5. Deployment complete!\n"

echo -e "......................................\n"

