#!/bin/sh

cat DLG-List | grep "^[a-mA-M].*" > DLG-Listam
cat DLG-List | grep "^[n-zN-Z].*" > DLG-Listnz

cat DLG-List | grep "^[a-mA-M].*" | sed "s%^%orig/%g"  > DLG-List-origam
cat DLG-List | grep "^[n-zN-Z].*" | sed "s%^%orig/%g"  > DLG-List-orignz

cat DLG-List | grep "^[a-mA-M].*" | sed "s/\.[dD][lL][gG]/.d/g" | sed "s%^%orig/%g"  > D-List-origam
cat DLG-List | grep "^[n-zN-Z].*" | sed "s/\.[dD][lL][gG]/.d/g" | sed "s%^%orig/%g"  > D-List-orignz

cat DLG-List | sed "s/\.[dD][lL][gG]/.d/g"  | grep "^[a-mA-M].*" > D-Listam
cat DLG-List | sed "s/\.[dD][lL][gG]/.d/g"  | grep "^[n-zN-Z].*" > D-Listnz

rm `cat D-Listam` 2&> /dev/null
rm `cat D-Listnz` 2&> /dev/null

rm `cat DLG-Listam` 2&> /dev/null
rm `cat DLG-Listnz` 2&> /dev/null

rm `cat D-List-origam` 2&> /dev/null
rm `cat D-List-orignz` 2&> /dev/null

rm `cat DLG-List-origam` 2&> /dev/null
rm `cat DLG-List-orignz` 2&> /dev/null

rm D-List*
rm DLG-Listam
rm DLG-Listnz
rm DLG-List-orig*
rm output*
