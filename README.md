# S-OS EMUZ80
S-OS SWORDはマイコン雑誌Oh!MZで発表されたZ80用のOSです。  
S-OSに蓄積された豊富なプログラム言語の一部をEMUZ80で動かせるようにしました。  



## 対象ハードウェア
S-OS for SuperMEZ80 (EMUZ80+MEZ80RAM)

## Assembler  
Macro Assembler 1.42 Beta [Bld 229]  
http://john.ccac.rwth-aachen.de:8000/as/  

```
asl -L unimon.asm
```

## 未対応のS-OSサブルーチン
プリンタ、ファイル関係、特殊ワークエリア、共通I/Oポート、カーソル操作＆情報  

## S-OSソフトウェア
S-OSのページ THE SENTINEL  
http://www.retropc.net/ohishi/s-os/index.html  

Lisp-85  
Prolog-85  
magiFORTH  
FuzzyBASIC  
STACKインタプリタ  
