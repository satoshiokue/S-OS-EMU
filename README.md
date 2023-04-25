# S-OS EMUZ80
S-OS SWORDはマイコン雑誌Oh!MZで発表されたZ80用のOSです。  
S-OS EMUZ80はS-OSに蓄積された豊富なプログラム言語の一部をEMUZ80で動かすことができます。  

EMUZ80用のモニタにはUniversal Monitorを拡張したEMUZ80-MON のRev.B02相当を使用しています。  
https://github.com/akih-san/EMUZ80-MON  

![S-OS](https://github.com/satoshiokue/S-OS-EMUZ80/blob/main/HELP.jpeg)

## 対象ハードウェア
SuperMEZ80 (EMUZ80+MEZ80RAM)  
https://github.com/satoshiokue/SuperMEZ80  

## 使用するアセンブラ  
Macro Assembler 1.42 Beta [Bld 229]  
http://john.ccac.rwth-aachen.de:8000/as/  

S-OS.asmとunimon.asmを同じフォルダに格納してアセンブルしてください。
```
asl -L unimon.asm
```

## 未実装のS-OSサブルーチン
プリンタ、ファイル関係、特殊ワークエリア、共通I/Oポート、カーソル操作＆情報  

## S-OSソフトウェア
S-OSソフトウェアはモニターから読み込む方法とEMUZ80のPICにあらかじめ書き込みロードする二つの方法があります。  

S-OSのページ THE SENTINEL  
http://www.retropc.net/ohishi/s-os/index.html  

Lisp-85  
Prolog-85  
magiFORTH  
FuzzyBASIC  
STACKインタプリタ  

プログラム言語のバイナリコードを配列データ化してmain.cの配列BASIC[]、LISP[]、FORTH[]、PROLOG[]、STACK[]に格納するとZ80のOUT命令で0x3000に転送することができます。  
またS-OSモニタのコマンドでプログラムをロードすることもできます。 

ファームウェアの書き込みはSuperMEZ80リポジトリで確認してください。  

## ライセンス
EMUZ80ファームウェアのmain.c  
GNU General Public License v3.0  

その他のコード  
MIT License  
