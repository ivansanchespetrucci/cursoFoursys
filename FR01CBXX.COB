      *=========================================*                       00001006
       IDENTIFICATION                            DIVISION.              00010000
      *=========================================*                       00011006
                                                                        00012006
       PROGRAM-ID.  FR01CBXX.                                           00020008
                                                                        00021006
      *=======================================================*         00021106
      *     AUTOR   : IVAN SANCHES                                      00022000
      *     EMPRESA : FOURSYS                                           00023001
      *-------------------------------------------------------*         00023106
      *     DATA   : ZZ/ZZ/ZZZ                                          00024000
      *     OBJETIVO: GRAVACAO DA ARQUIVO FUNC                          00025019
      *=======================================================*         00025106
                                                                        00025206
      *=======================================================*         00025306
       ENVIRONMENT                               DIVISION.              00026006
      *=======================================================*         00026106
       INPUT-OUTPUT                              SECTION.               00026709
       FILE-CONTROL.                                                    00026809
             SELECT PRODUTO  ASSIGN TO ARQPROD                          00026920
                FILE STATUS  IS WRK-FS-PRODUTO.                         00027020
                                                                        00027120
             SELECT RESUMO   ASSIGN TO ARQRES                           00027220
                FILE STATUS  IS WRK-FS-RESUMO.                          00027320
      *=======================================================*         00027706
       DATA                                      DIVISION.              00027800
      *=======================================================*         00027906
       FILE                                      SECTION.               00028009
       FD PRODUTO                                                       00028120
           RECORDING MODE IS F                                          00028210
           BLOCK CONTAINS 0 RECORDS.                                    00028310
       01 FD-PRODUTO  PIC X(70).                                        00028520
                                                                        00028620
       FD RESUMO                                                        00028720
           RECORDING MODE IS F                                          00028820
           BLOCK CONTAINS 0 RECORDS.                                    00028920
       01 FD-RESUMO   PIC X(40).                                        00029020
      *---------------------------------------------------              00029113
       WORKING-STORAGE                           SECTION.               00029213
      *---------------------------------------------------              00029313
                                                                        00029425
      * AREA ARQUIVO PRODUTO - BOOK                                     00029527
           COPY 'B#PROD'.                                               00029627
                                                                        00029727
       01 WRK-REG-RESUMO.                                               00029829
          05 WRK-REG-COD               PIC 9(05)    VALUE ZEROS.        00029929
          05 WRK-REG-PROD              PIC X(20)    VALUE SPACES.       00030029
          05 WRK-REG-FORN              PIC X(15)    VALUE SPACES.       00030129
      * AREA VARIAVEIS FILE-STATUS                                      00030227
                                                                        00030327
       77 WRK-FS-PRODUTO  PIC X(02) VALUE SPACES.                       00030420
       77 WRK-FS-RESUMO   PIC X(02) VALUE SPACES.                       00030520
                                                                        00030627
      * AREA VARIAVEIS AUXILIAR                                         00030727
       77 WRK-MSG         PIC X(50) VALUE SPACES.                       00030820
                                                                        00030927
      *=========================================*                       00031017
       PROCEDURE DIVISION.                                              00031117
      *=========================================*                       00031217
                                                                        00031317
      *---------------------------------------------------              00031417
       0000-PRINCIPAL                     SECTION.                      00031517
      *---------------------------------------------------              00031617
           PERFORM 0100-INICIAR.                                        00031717
           PERFORM 0200-PROCESSAR UNTIL WRK-FS-PRODUTO EQUAL '10'       00031820
           PERFORM 0300-FINALIZAR.                                      00031917
      *---------------------------------------------------              00032020
       0000-99-FIM.            EXIT.                                    00032120
      *---------------------------------------------------              00032220
                                                                        00032317
       0100-INICIAR                       SECTION.                      00032417
            OPEN INPUT PRODUTO                                          00032520
                 OUTPUT RESUMO.                                         00032620
              PERFORM 0110-TESTAR-STATUS.                               00032720
               READ PRODUTO INTO WRK-REG-PRODUTO.                       00032825
       0110-TESTAR-STATUS                 SECTION.                      00032920
            IF WRK-FS-PRODUTO NOT EQUAL ZEROS                           00033020
                 MOVE ' ERRO OPEN PRODUTO ' TO WRK-MSG                  00034320
                   PERFORM 9000-TRATA-ERROS                             00034420
             END-IF.                                                    00035020
            IF WRK-FS-RESUMO NOT EQUAL ZEROS                            00036020
                 MOVE ' ERRO OPEN RESUMO ' TO WRK-MSG                   00037020
                   PERFORM 9000-TRATA-ERROS                             00038020
             END-IF.                                                    00039020
       0200-PROCESSAR                             SECTION.              00061020
              IF WRK-FS-PRODUTO EQUAL ZEROS                             00063022
                  MOVE WRK-REG-PRODUTO(1:40) TO FD-RESUMO               00064025
                    MOVE  FD-RESUMO TO WRK-REG-RESUMO                   00064128
                   WRITE FD-RESUMO FROM WRK-REG-RESUMO                  00065029
                    DISPLAY 'GRAVEI ' WRK-REG-RESUMO                    00065128
              ELSE                                                      00065220
                 MOVE ' FINAL DE ARQUIVO ' TO WRK-MSG                   00065320
                                                                        00065420
             END-IF.                                                    00066020
             READ PRODUTO INTO WRK-REG-PRODUTO.                         00066125
       0300-FINALIZAR                          SECTION.                 00067020
                   CLOSE PRODUTO                                        00068020
                   CLOSE RESUMO                                         00069020
                    MOVE 'FINAL DE PROCESSO' TO WRK-MSG                 00069120
                    PERFORM 9000-TRATA-ERROS.                           00069221
      *---------------------------------------------------              00070006
       9000-TRATA-ERROS                  SECTION.                       00080006
      *---------------------------------------------------              00090006
             DISPLAY '------------------'.                              00091012
             DISPLAY '  MENSAGEM        '  WRK-MSG.                     00091112
               STOP RUN.                                                00091220
      *--------------------------------------------------------------   00092012
       9000-99-FIM.            EXIT.                                    00093007
      *--------------------------------------------------------------   00094006
                                                                        00095006
