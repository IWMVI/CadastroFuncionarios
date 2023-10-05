      *************************************************************************
      * AUTHOR. WALLACE MARTINS VIEIRA
      * DATA : 04/10/2023
      * CADASTRO FUNCIONÁRIOS.
      *************************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGR231.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADFUNC ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE IS DYNAMIC
                      RECORD KEY IS NOME
                      ALTERNATE RECORD KEY IS EMAIL WITH DUPLICATES
                      FILE STATUS IS ST-ERRO.
       DATA DIVISION.
       FILE SECTION.
       FD CADFUNC
          LABEL RECORD IS STANDARD
          VALUE OF FILE-ID IS "REGFUNC.DAT".

       01 REGFUNC.
        03 CHAVE1.
          05 NOME             PIC X(30).
        03 DEPARTAMENTO       PIC 9(02).
        03 EMAIL              PIC X(30).
        03 CARGO              PIC 9(02).
        03 DATAADMISSAO       PIC 9(10).
        03 SEXO               PIC X(01).
        03 SALARIOBASE        PIC 9(06)V99.
        03 CODIGO             PIC X(06).
        03 ADMISSAO           PIC X(10).
        03 TPSALARIO          PIC X(01).

       WORKING-STORAGE SECTION.
       01 W-OPCAO             PIC X(01) VALUE SPACES.
       01 W-ACT               PIC 9(02) VALUE ZEROS.
       01 ST-ERRO             PIC X(02) VALUE "00".
       01 W-CONT              PIC 9(06) VALUE ZEROS.
       01 MENSAGEM            PIC X(65) VALUE SPACES.

       01 W-SEL               PIC 9(01) VALUE ZEROS.

       01 TABFUNC.
        05 TBCARGO        PIC X(30) OCCURS 10 TIMES.

       SCREEN SECTION.

       01  TELACADFUNC.
           05  LINE 01  COLUMN 01
               VALUE  "========================================".
           05  LINE 01  COLUMN 41
               VALUE  "========================================".
           05  LINE 02  COLUMN 01
               VALUE  "                              CADASTRO D".
           05  LINE 02  COLUMN 41
               VALUE  "E FUNCIONARIOS".
           05  LINE 03  COLUMN 01
               VALUE  "========================================".
           05  LINE 03  COLUMN 41
               VALUE  "========================================".
           05  LINE 05  COLUMN 01
               VALUE  "NOME :".
           05  LINE 05  COLUMN 41
               VALUE  "  EMAIL :".
           05  LINE 07  COLUMN 01
               VALUE  "SEXO :   -".
           05  LINE 07  COLUMN 41
               VALUE  "  CARGO :    -".
           05  LINE 09  COLUMN 01
               VALUE  "DPTO :    -".
           05  LINE 09  COLUMN 41
               VALUE  "  CODIGO:".
           05  LINE 11  COLUMN 01
               VALUE  "========================================".
           05  LINE 11  COLUMN 41
               VALUE  "========================================".
           05  LINE 13  COLUMN 01
               VALUE  "DATA ADMISSAO :".
           05  LINE 13  COLUMN 41
               VALUE  " TIPO DE SALARIO :   -".
           05  LINE 15  COLUMN 41
               VALUE  " SALARIO BASE    :".
           05  LINE 17  COLUMN 01
               VALUE  "========================================".
           05  LINE 17  COLUMN 41
               VALUE  "========================================".
           05  LINE 22  COLUMN 01
               VALUE  "========================================".
           05  LINE 22  COLUMN 41
               VALUE  "========================================".
           05  LINE 23  COLUMN 01
               VALUE  "MENSAGEM :".
           05  LINE 24  COLUMN 01
               VALUE  "========================================".
           05  LINE 24  COLUMN 41
               VALUE  "========================================".
           05  WS-NOME
               LINE 05  COLUMN 08  PIC X(30)
               USING  NOME
               HIGHLIGHT     REQUIRED.
           05  WS-EMAIL
               LINE 05  COLUMN 51  PIC X(30)
               USING  EMAIL
               HIGHLIGHT.
           05  WS-SEXO
               LINE 07  COLUMN 08  PIC X(01)
               USING  SEXO
               HIGHLIGHT.
           05  WS-CARGO
               LINE 07  COLUMN 51  PIC X(02)
               USING  CARGO
               HIGHLIGHT.
           05  WS-DEPARTAMENTO
               LINE 09  COLUMN 08  PIC 9(02)
               USING  DEPARTAMENTO
               HIGHLIGHT.
           05  WS-CODIGO
               LINE 09  COLUMN 51  PIC X(06)
               USING  CODIGO
               HIGHLIGHT.
           05  WS-ADMISSAO
               LINE 13  COLUMN 17  PIC XX/XX/XXXX
               USING  ADMISSAO
               HIGHLIGHT.
           05  WS-TPSALARIO
               LINE 13  COLUMN 60  PIC X(01)
               USING  TPSALARIO
               HIGHLIGHT.
           05  WS-SALARIOBASE
               LINE 15  COLUMN 60  PIC 9(06)
               USING  SALARIOBASE
               HIGHLIGHT.
           05  WS-MENSAGEM
               LINE 23  COLUMN 12  PIC X(65)
               USING  MENSAGEM
               HIGHLIGHT.

       01  TELACARGOS.
           05  LINE 17  COLUMN 01
               VALUE  "========================================".
           05  LINE 17  COLUMN 41
               VALUE  "========================================".
           05  LINE 19  COLUMN 01
               VALUE  "      01 - ADMINISTRACAO      04 - MANUT".
           05  LINE 19  COLUMN 41
               VALUE  "ENCAO         07 - RECEPCIONISTA".
           05  LINE 20  COLUMN 01
               VALUE  "      02 - ADVOGADO           05 - FINCA".
           05  LINE 20  COLUMN 41
               VALUE  "NCEIRO        08 - MOTORISTA".
           05  LINE 21  COLUMN 01
               VALUE  "      03 - SECRETARIA         06 - REPRE".
           05  LINE 21  COLUMN 41
               VALUE  "SENTANTE      09 - T.I".
           05  LINE 22  COLUMN 01
               VALUE  "========================================".
           05  LINE 22  COLUMN 41
               VALUE  "========================================".

       01 TELALIMPAR.

           05  LINE 17  COLUMN 01
               VALUE  "========================================".
           05  LINE 17  COLUMN 41
               VALUE  "========================================".
           05  LINE 19  COLUMN 01
               VALUE  "                                               ".
           05  LINE 19  COLUMN 41
               VALUE  "                                               ".
           05  LINE 20  COLUMN 01
               VALUE  "                                               ".
           05  LINE 20  COLUMN 41
               VALUE  "                                               ".
           05  LINE 21  COLUMN 01
               VALUE  "                                               ".
           05  LINE 21  COLUMN 41
               VALUE  "                                               ".
           05  LINE 23 COLUMN 12
               VALUE  "                                               ".
           05  LINE 23 COLUMN 40
               VALUE  "                                         ".
           05  LINE 22  COLUMN 01
               VALUE  "========================================".
           05  LINE 22  COLUMN 41
               VALUE  "========================================".

       PROCEDURE DIVISION.
       INICIO.
            MOVE "ADMINISTRACAO" TO TBCARGO(1).
            MOVE "ADVOGADO"      TO TBCARGO(2).
            MOVE "SECRETARIA"    TO TBCARGO(3).
            MOVE "MANUTENCAO"    TO TBCARGO(4).
            MOVE "FINCANCEIRO"   TO TBCARGO(5).
            MOVE "REPRESENTANTE" TO TBCARGO(6).
            MOVE "ESTAGIARIO"    TO TBCARGO(7).
            MOVE "RECEPCIONISTA" TO TBCARGO(8).
            MOVE "MOTORISTA"     TO TBCARGO(9).
            MOVE "T.I"           TO TBCARGO(10).

       R0.
            OPEN I-O CADFUNC
            IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                   OPEN OUTPUT CADFUNC
                   MOVE "CRIANDO ARQUIVO" TO MENSAGEM
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   CLOSE CADFUNC
                   GO TO INICIO
               ELSE
                   IF ST-ERRO = "95"
                       MOVE "ISAM NAO CARREGADO" TO MENSAGEM
                       PERFORM ROT-MENS THRU ROT-MENS-FIM
                       GO TO ROT-FIM
                   ELSE
                       MOVE "ERRO NA ABERTURA DO ARQUIVO" TO MENSAGEM
                       PERFORM ROT-MENS THRU ROT-MENS-FIM
                       GO TO ROT-FIM.
       R1.
            MOVE SPACES TO NOME EMAIL SEXO TPSALARIO
            CODIGO
            MOVE ZEROS TO DEPARTAMENTO CODIGO SALARIOBASE CARGO.

       RTELA.
            DISPLAY TELACADFUNC.

      ************************ ENTRADA DE DADOS ************************

       R2.
            ACCEPT WS-NOME
            ACCEPT W-ACT FROM ESCAPE KEY
            IF W-ACT = 01
                CLOSE CADFUNC
                STOP RUN.
            IF NOME = SPACES
                MOVE "NOME NAO PODE FICAR EM BRANCO" TO MENSAGEM
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R2.

       LER-CADFUNC.
            READ CADFUNC
            IF ST-ERRO NOT = "23"
                IF ST-ERRO = "00"
                   DISPLAY TELACADFUNC
                   MOVE "FUNCIONARIO JA CADASTRADO" TO MENSAGEM
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ACE-001
                ELSE
                    MOVE "ERRO NA LEITURA DO ARQUIVO" TO MENSAGEM
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
            ELSE
                MOVE "FUNCIONARIO NAO CADASTRADO" TO MENSAGEM
                PERFORM ROT-MENS THRU ROT-MENS-FIM.

       R3.
            ACCEPT WS-EMAIL
            ACCEPT W-ACT FROM ESCAPE KEY
            IF W-ACT = 01
               GO TO R2.
            IF EMAIL = SPACES
                MOVE "EMAIL NAO PODE FICAR EM BRANCO" TO MENSAGEM
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R3.

       R4.
            ACCEPT WS-SEXO
            ACCEPT W-ACT FROM ESCAPE KEY
            IF W-ACT = 01
                GO TO R3
            IF SEXO = SPACES
                MOVE "SEXO NAO PODE FICAR EM BRANCO" TO MENSAGEM
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R4.

       R5.
            DISPLAY TELACARGOS
            ACCEPT WS-CARGO
            ACCEPT W-ACT FROM ESCAPE KEY
            IF W-ACT = 01
                GO TO R4
            IF CARGO = ZEROS
                MOVE "CARGO NAO PODE FICAR EM BRANCO" TO MENSAGEM
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R5.

       R5A.
            DISPLAY TELALIMPAR.

       R6.
            ACCEPT WS-DEPARTAMENTO
            ACCEPT W-ACT FROM ESCAPE KEY
            IF W-ACT = 01
                GO TO R5.
            IF DEPARTAMENTO = ZEROS
                MOVE "DEPARTAMENTO NAO PODE FICAR EM BRANCO" TO MENSAGEM
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R6.

       R7.
            ACCEPT WS-CODIGO
            ACCEPT W-ACT FROM ESCAPE KEY
            IF W-ACT = 01
                GO TO R6.
            IF CODIGO = ZEROS
                MOVE "CODIGO NAO PODE FICAR EM BRANCO" TO MENSAGEM
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R7.

       R8.
            ACCEPT WS-ADMISSAO
            ACCEPT W-ACT FROM ESCAPE KEY
            IF W-ACT = 01
                GO TO R7
            IF ADMISSAO = SPACES
                MOVE "ADMISSAO NAO PODE FICAR EM BRANCO" TO MENSAGEM
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R8.

       R9.
            ACCEPT WS-TPSALARIO
            ACCEPT W-ACT FROM ESCAPE KEY
            IF W-ACT = 01
                GO TO R8.
            IF TPSALARIO = SPACES
                MOVE "TIPO SALARIO NAO PODE FICAR EM BRANCO" TO MENSAGEM
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R9.

       R10.
            ACCEPT WS-SALARIOBASE
            ACCEPT W-ACT FROM ESCAPE KEY
            IF W-ACT = 01
                GO TO R9
            IF SALARIOBASE = ZEROS
                MOVE "SALARIO BASE NAO PODE FICAR EM BRANCO" TO MENSAGEM
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R10.

       RLIMPAR.
            DISPLAY TELALIMPAR.


       INC-OPC.
            DISPLAY (23, 12) "DESEJA SALVAR (S/N): ".
            ACCEPT W-OPCAO
            ACCEPT W-ACT FROM ESCAPE KEY
            IF W-ACT = 01
                GO TO R10.
            IF W-OPCAO = "N" OR "n"
                MOVE "DADOS NAO FORAM SALVOS" TO MENSAGEM
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R1.
            IF W-OPCAO NOT = "S" AND W-OPCAO NOT = "s"
                MOVE "DIGITE APENAS S = SIM OU N = NAO" TO MENSAGEM
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO INC-OPC.

       INC-WR1.
            WRITE REGFUNC
            IF ST-ERRO = "00" OR ST-ERRO = "02"
                MOVE "DADOS GRAVADOS" TO MENSAGEM
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R1.
            IF ST-ERRO = "22"
                MOVE "FUNCIONARIO JA CADASTRADO" TO MENSAGEM
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R1
            ELSE
                MOVE "ERRO NA GRAVACAO DO FUNCIONARIO" TO MENSAGEM
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

      ******************************************************************
      *                        ROTINAS DE ALTERAÇÃO
      ******************************************************************

       ACE-001.
            DISPLAY TELALIMPAR
            DISPLAY (23, 12)
               "N = NOVO REGISTRO A = ALTERAR E = EXCLUIR - OPCAO : "
            ACCEPT W-OPCAO
            IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A"
                AND W-OPCAO NOT = "E"
                GO TO ACE-001.
            MOVE SPACES TO MENSAGEM
            IF W-OPCAO = "N"
                GO TO R1
            ELSE
                IF W-OPCAO = "A"
                   MOVE 1 TO W-SEL
                   GO TO R3
            ELSE
                IF W-OPCAO = "S"
                    MOVE 1 TO W-SEL
                    GO TO ROT-FIM
             DISPLAY TELALIMPAR.

       EXC-OPC.
            DISPLAY TELALIMPAR
            DISPLAY (23, 12) "EXCLUIR (S/N): "
            ACCEPT W-OPCAO
            IF W-OPCAO = "N" OR "n"
                MOVE "REGISTRO MANTIDO" TO MENSAGEM
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R1.
            IF W-OPCAO NOT = "S" AND "s"
                MOVE "DIGITE APENAS S = SIM OU N = NAO" TO MENSAGEM
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO EXC-OPC.

       EXC-DL1.
            DELETE CADFUNC RECORD
            IF ST-ERRO = "00"
                MOVE "FUNCIONARIO EXCLUIDO" TO MENSAGEM
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R1.
            MOVE "ERRO NA EXCLUSAO DO REGISTRO" TO MENSAGEM
            PERFORM ROT-MENS THRU ROT-MENS-FIM
            GO TO ROT-FIM.

       ALT-OPC.
            DISPLAY (23, 12) "ALTERAR (S/N): "
            ACCEPT (23, 27) W-OPCAO
            ACCEPT W-ACT FROM ESCAPE KEY

            IF W-ACT = "N" OR "n"
                MOVE "INFORMACOES MANTIDAS" TO MENSAGEM
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R1.
            IF W-ACT NOT = "S" OR "s"
                MOVE "DIGITE APENAS S = SIM OU N = NAO" TO MENSAGEM
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ALT-OPC.

       ALT-RW1.
            REWRITE REGFUNC
            IF ST-ERRO = "00" OR ST-ERRO = "02"
                MOVE "REGISTRO ALTERADO" TO MENSAGEM
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R1.
                DISPLAY TELALIMPAR
            MOVE "ERRO NA ALTERACAO DO ARQUIVO" TO MENSAGEM
            PERFORM ROT-MENS THRU ROT-MENS-FIM
            GO TO ROT-FIM.

       ROT-FIM.
            CLOSE CADFUNC.
            STOP RUN.
      ******************************************************************
      *                        ROTINAS DE MENSAGENS
      ******************************************************************

       ROT-MENS.
            MOVE ZEROS TO W-CONT.
       ROT-MENS1.
            DISPLAY (23, 12) MENSAGEM.
       ROT-MENS2.
            ADD 1 TO W-CONT
            IF W-CONT < 3000
                GO TO ROT-MENS2
            ELSE
                MOVE SPACES TO MENSAGEM
                DISPLAY (23, 12) MENSAGEM.
       ROT-MENS-FIM.
            EXIT.
       ROT-FIM-TEMPO.
