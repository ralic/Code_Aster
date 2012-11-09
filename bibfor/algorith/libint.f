      SUBROUTINE LIBINT(IMPED,NUME91,NBINT,LISINT,NBEQ1)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.
C
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
       IMPLICIT NONE

C---------------------------------------------------------------C
C--       ROUTINE XXXXX3        M. CORUS - AOUT 2011          --C
C--       CONSTRUCTION DE LA MATRICE LIBRE A L'INTERFACE      --C
C--                                                           --C
C--   REALISE APRES TEST DE LA METHODE DECRITE DANS LA DOC R3 --C
C--   (QUI MARCHE PAS) CONSSITANT A JOUER SUR LES COEFF DES   --C
C--   MULTIPLICATEURS DE LAGRANGE                             --C
C--                                                           --C
C--                                                           --C
C--       METHODE PERSO : ON VIRE LES LAGRANGES :             --C
C--          K(IND_LAG,IND_LAG)=IDENTITE*COEFF_LAGRANGE       --C
C--          K(IND_INTERF,IND_LAG)=0                          --C
C--  ET DONC K(IND_LAG,IND_INTERF)=0                          --C
C--                                                           --C
C--  AVANTAGES : ON CONSERVE LA NUMROTATION                   --C
C--              ON PEUT "MIXER" DES RESULTATS                --C
C--                  INTERFACE LIBRE / INTERFACE FIXE         --C
C--                                                           --C
C--  INCONVENIENT : ON RISQUE DE FAIRE N'IMPORTE QUOI SI      --C
C--                 ON NE FAIT PAS UN PEU ATTENTION           --C
C--                                                           --C
C-- RECOMMANDATION : N'UTILISER CETTE ROUTINE QUE SUR UNE     --C
C--                  COPIE DE LA MATICE DE RAIDEUR INITIALE   --C
C--                                                           --C
C--                                                           --C
C-- APRES TEST DE LA METHODE DECRITE DANS LA DOC R3           --C
C-- (QUI MARCHE PAS) CONSSITANT A JOUER SUR LES COEFF DES     --C
C-- MULTIPLICATEURS DE LAGRANGE                               --C
C--                                                           --C
C---------------------------------------------------------------C
C--   VARIABLES E/S  :
C--   IMPED    /IN/  : NOM K19 DE LA MATRICE DE RAIDEUR
C--   NUME91   /IN/  : NOM DU NUME_DDL ASSOCIE
C--   NBINT    /IN/  : NOMBRE D'INTERFACE DONT IL FAUT LIBERER
C--                       LES LAGRANGES
C--   LISINT   /IN/  : LISTE DES NOMS D'INTERFACES PERMETTANT DE
C--                       RECUPERER LES DDL CONCERNES PAR LA LIBERAtION
C--                       VOIR DANS LE CODE ET DANS OP0091 POUR UNE
C--                       UTILISATION DANS UN AUTRE CADRE
C--   NBEQ1    /IN/  : NB DE DDL DE LA MATRICE
C
C

C
C
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM
      CHARACTER*8  KB
      CHARACTER*19 IMPED,NUME91
      CHARACTER*24 INDIN1
      INTEGER      J1,K1,L1,M1,N1,NBEQ1,LLINT1,NBDDL1,LINTF,NBINT,
     &             LKLIBR,LNUME,LAG1,LAG2,IND,LSMHC,LDELG
      REAL*8       ABS
      CHARACTER*24 LISINT

      CALL JEVEUO(LISINT,'L',LINTF)
C-- RECUPERATION DE LA MATRICE DE RAIDEUR
      CALL JEVEUO(JEXNUM(IMPED(1:19)//'.VALM',1),'E',LKLIBR)

C-- RECUPERATION DES INFOS DU NUME_DDL
      CALL JEVEUO(NUME91(1:14)//'.SMOS.SMDI','L',LNUME)
      CALL JEVEUO(NUME91(1:14)//'.SMOS.SMHC','L',LSMHC)
      CALL JEVEUO(NUME91(1:14)//'.NUME.DELG','L',LDELG)


      DO 180 K1=1,NBINT

        INDIN1='&&VEC_DDL_INTF_'//ZK8(LINTF+K1-1)
        CALL JEVEUO(INDIN1,'L',LLINT1)
        CALL JELIRA(INDIN1,'LONMAX',NBDDL1,KB)

        DO 190 M1=1,NBDDL1
          IF (ZI(LLINT1+M1-1) .GT. 0) THEN
            CALL DDLLAG(NUME91,ZI(LLINT1+M1-1),NBEQ1,LAG1,LAG2)
C-- SUPRESSION DES COUPLAGES L1 / L2
            IF (LAG1 .GT. 1) THEN
              L1=ZI(LNUME+LAG1-1)-ZI(LNUME+LAG1-2)-1
              IND=ZI(LNUME+LAG1-2)
              DO 230 N1=1,L1
                ZR(LKLIBR+IND+N1-1)=0.D0
  230         CONTINUE
            ENDIF
            IF (LAG2 .GT. 1) THEN
              L1=ZI(LNUME+LAG2-1)-ZI(LNUME+LAG2-2)-1
              IND=ZI(LNUME+LAG2-2)
              DO 240 N1=1,L1
                ZR(LKLIBR+IND+N1-1)=0.D0
  240         CONTINUE
              ENDIF

C-- SUPPRESSION DES COUPLAGES EQ / L1
            IF (ZI(LLINT1+M1-1) .GT. 1) THEN
              L1=ZI(LNUME+ZI(LLINT1+M1-1)-1)-
     &           ZI(LNUME+ZI(LLINT1+M1-1)-2)-1
              IND=ZI(LNUME+ZI(LLINT1+M1-1)-2)
              DO 250 J1=1,L1
C-- ON TESTE DANS LE NUME.DELG SI LA VALEUR EST NEGATIVE
                IF ( ZI(LDELG+ZI4(LSMHC+IND+J1-1)-1) .LT. 0) THEN
                  ZR(LKLIBR+IND+J1-1)=0.D0
                ENDIF
  250         CONTINUE
            ENDIF
C-- ON REND LA DIAGONALE POSITIVE
            ZR(LKLIBR+ZI(LNUME+LAG1-1)-1)=
     &        ABS(ZR(LKLIBR+ZI(LNUME+LAG1-1)-1))
            ZR(LKLIBR+ZI(LNUME+LAG2-1)-1)=
     &        ABS(ZR(LKLIBR+ZI(LNUME+LAG2-1)-1))
          ENDIF

  190   CONTINUE

  180 CONTINUE

      END
