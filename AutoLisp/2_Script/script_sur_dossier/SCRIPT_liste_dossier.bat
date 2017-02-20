echo off

If exist script.scr del script.scr

dir *.dwg /b /s /on>tmp1.txt

type tmp1.txt|sort>liste.txt

del tmp1.txt

dir *.dxf /b /s /on>tmp1.txt

type tmp1.txt|sort>tmp2.txt

del tmp1.txt

for /f "delims=" %%i in ('type tmp2.txt') do (
echo %%i>>liste.txt
)

del tmp2.txt

for /f "delims=" %%i in ('type liste.txt') do (
echo _open>>script.scr
echo ^"%%i^">>script.scr
for /f "delims=" %%j in ('type cmd.txt') do (
echo %%j>>script.scr
)
echo _close>>script.scr
)

del liste.txt





