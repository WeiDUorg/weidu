#!/bin/sh
cat DLG-List | grep "^[a-mA-M].*" > DLG-Listam
cat DLG-List | grep "^[n-zN-Z].*" > DLG-Listnz

cat DLG-List | grep "^[a-mA-M].*" | sed "s%^%orig/%g"  > DLG-List-origam
cat DLG-List | grep "^[n-zN-Z].*" | sed "s%^%orig/%g"  > DLG-List-orignz

cat DLG-List | grep "^[a-mA-M].*" | sed "s/\.[dD][lL][gG]/.d/g" | sed "s%^%orig/%g"  > D-List-origam
cat DLG-List | grep "^[n-zN-Z].*" | sed "s/\.[dD][lL][gG]/.d/g" | sed "s%^%orig/%g"  > D-List-orignz

cat DLG-List | sed "s/\.[dD][lL][gG]/.d/g"  | grep "^[a-mA-M].*" > D-Listam
cat DLG-List | sed "s/\.[dD][lL][gG]/.d/g"  | grep "^[n-zN-Z].*" > D-Listnz

weidu.exe --out orig --biff-get-rest `cat DLG-Listam` >> output.1 ;
weidu.exe --out orig --biff-get-rest `cat DLG-Listnz` >> output.1 ;

weidu.exe --nofrom --noheader --nocom `cat DLG-List-origam` >> output.2
weidu.exe --nofrom --noheader --nocom `cat DLG-List-orignz` >> output.2

weidu.exe `cat D-Listam` >> output.3
weidu.exe `cat D-Listnz` >> output.3

for i in `cat DLG-Listam` ; do cmp $i orig/$i || echo "Failure : $i" ; done
for i in `cat DLG-Listnz` ; do cmp $i orig/$i || echo "Failure : $i" ; done
