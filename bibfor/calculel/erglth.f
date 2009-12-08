      SUBROUTINE ERGLTH (CHAMP,INST,NIVEAU,IORDR,RESUCO)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 11/03/2008   AUTEUR MEUNIER S.MEUNIER 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C =====================================================================
C ERREUR GLOBALE AU MAILLAGE - THERMIQUE
C **     **                    **
C =====================================================================
C     BUT :  EN THERMIQUE, CALCULER LES ESTIMATEURS GLOBAUX
C            A PARTIR DES ESTIMATEURS LOCAUX CONTENUS DANS CHAMP
C
C IN  CHAMP    :  NOM DU CHAM_ELEM_ERREUR
C IN  INSTANT  :  INSTANT DE CALCUL
C IN  NIVEAU   :  NIVEAU DE L'ESTIMATEUR
C IN  IORDR    :  NUMERO D'ORDRE
C IN  RESUCO   :  SD RESULTAT.
C   -------------------------------------------------------------------
C     SUBROUTINES APPELLEES:
C       MESSAGE : INFNIV.
C       JEVEUX  : JEMARQ,JELIRA,JEVEUO,JEDEMA.
C       ASTER   : IUNIFI,CELVER.
C       ENVIMA  : R8MIEM.
C
C     FONCTIONS INTRINSEQUES:
C       ABS,SQRT.
C  -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       05/07/01 (OB) : CREATION EN S'INSPIRANT DE ERGLOB.F.
C       12/09/02 (OB) : MODIF. MSG D'ALARME DE LA DIVISION PAR ZERO.
C --------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      REAL*8         INST
      INTEGER        NIVEAU,IORDR
      CHARACTER*8    RESUCO
      CHARACTER*(*)  CHAMP

C ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C ------------------------------------------------------------------

C DECLARATION VARIABLES LOCALES
      INTEGER IFI,NBGREL,NBELEM,DIGDEL,LONGT,LONG2,MODE,J,IBID,NBGR,
     &        IACELK,ICOEF,NEL,IDECGR,K,IAD,IAVALE,NBEL,JCELD,IUNIFI
      REAL*8 TERMVO,TERMSA,TERMFL,TERMEC,OVFL,R8MIEM,TERMS1,
     &  TERMF1,TERME1,TERMV1,TERMV2,TERMS2,TERMF2,TERME2,ERR0,
     &  NORS,NU0
      CHARACTER*4  DOCU
      CHARACTER*19 CHAMP2,LIGREL
      LOGICAL FIRST

C INIT.
      CALL JEMARQ()
      OVFL = R8MIEM()
      IFI = IUNIFI('RESULTAT')
      CHAMP2 = CHAMP

C ON RETROUVE LE NOM DU LIGREL:
C     -- ON VERIFIE QUE LE CHAM_ELEM N'EST PAS TROP DYNAMIQUE :
      CALL CELVER(CHAMP2,'NBVARI_CST','STOP',IBID)
      CALL CELVER(CHAMP2,'NBSPT_1','STOP',IBID)
      CALL JELIRA (CHAMP2//'.CELD','DOCU',IBID,DOCU)
      IF(DOCU.NE.'CHML')
     & CALL U2MESS('F','CALCULEL5_44')
      CALL JEVEUO (CHAMP2//'.CELK','L',IACELK)
      LIGREL = ZK24(IACELK-1+1)(1:19)

      CALL JEVEUO (CHAMP2//'.CELD','L',JCELD)

C     -- ON VERIFIE LES LONGUEURS:
      FIRST = .TRUE.
      NBGR  = NBGREL(LIGREL)
      DO 1 J = 1,NBGR
         MODE=ZI(JCELD-1+ZI(JCELD-1+4+J) +2)
         IF (MODE.EQ.0) GOTO 1
         LONG2 = DIGDEL(MODE)
         ICOEF=MAX(1,ZI(JCELD-1+4))
         LONG2 = LONG2 * ICOEF
         IF (FIRST) THEN
           LONGT = LONG2
         ELSE
           IF (LONGT.NE.LONG2)
     &       CALL U2MESS('F','CALCULEL5_45')
         ENDIF
         FIRST = .FALSE.
   1  CONTINUE

C        -- ON CUMULE :
         CALL JEVEUO (CHAMP2//'.CELV','E',IAVALE)

         ERR0 = 0.D0
         NORS = 0.D0
         NBEL = 0
         IF (NIVEAU.EQ.2) THEN
           TERMVO = 0.D0
           TERMSA = 0.D0
           TERMFL = 0.D0
           TERMEC = 0.D0
           TERMV1 = 0.D0
           TERMS1 = 0.D0
           TERMF1 = 0.D0
           TERME1 = 0.D0
           TERMV2 = 0.D0
           TERMS2 = 0.D0
           TERMF2 = 0.D0
           TERME2 = 0.D0
         ENDIF

         DO 2 J = 1,NBGR
            MODE=ZI(JCELD-1+ZI(JCELD-1+4+J) +2)
            IF (MODE.EQ.0 ) GOTO 2
            NEL = NBELEM(LIGREL,J)
            IDECGR=ZI(JCELD-1+ZI(JCELD-1+4+J)+8)
            DO 3 K = 1,NEL
              IAD  = IAVALE-1+IDECGR+(K-1)*LONGT
              ERR0 = ERR0 + ZR(IAD)**2
              NORS = NORS + ZR(IAD+2)**2
              IF (NIVEAU.EQ.2) THEN
                TERMVO = TERMVO + ZR(IAD+3)**2
                TERMV1 = TERMV1 + ZR(IAD+5)**2
                TERMSA = TERMSA + ZR(IAD+6)**2
                TERMS1 = TERMS1 + ZR(IAD+8)**2
                TERMFL = TERMFL + ZR(IAD+9)**2
                TERMF1 = TERMF1 + ZR(IAD+11)**2
                TERMEC = TERMEC + ZR(IAD+12)**2
                TERME1 = TERME1 + ZR(IAD+14)**2
              ENDIF
              NBEL = NBEL + 1
 3          CONTINUE
 2        CONTINUE
      ERR0 = SQRT(ERR0)
      NORS = SQRT(NORS)
      IF (NIVEAU.EQ.2) THEN
C ERREURS PARTIELLES ABSOLUES
        TERMVO = SQRT(TERMVO)
        TERMSA = SQRT(TERMSA)
        TERMFL = SQRT(TERMFL)
        TERMEC = SQRT(TERMEC)
        TERMV1 = SQRT(TERMV1)
        TERMS1 = SQRT(TERMS1)
        TERMF1 = SQRT(TERMF1)
        TERME1 = SQRT(TERME1)
C ERREURS PARTIELLES RELATIVES
        IF (TERMV1.GT.OVFL) TERMV2 = 100.D0*(TERMVO/TERMV1)
        IF (TERMS1.GT.OVFL) TERMS2 = 100.D0*(TERMSA/TERMS1)
        IF (TERMF1.GT.OVFL) TERMF2 = 100.D0*(TERMFL/TERMF1)
        IF (TERME1.GT.OVFL) TERME2 = 100.D0*(TERMEC/TERME1)
      ENDIF
      IF (NORS.GT.OVFL) THEN
        NU0 = 100.D0*ERR0/NORS
      ELSE
        CALL U2MESS('A','CALCULEL5_46')
        NU0 = 0.D0
      ENDIF
      WRITE(IFI,*) ' '
      WRITE(IFI,*) '**********************************************'
      WRITE(IFI,*) ' THERMIQUE: ESTIMATEUR D''ERREUR EN RESIDU '
      WRITE(IFI,*) '**********************************************'
      WRITE(IFI,*)
      WRITE(IFI,*) '   IMPRESSION DES NORMES GLOBALES :'
      WRITE(IFI,*)

C ESTIMATEURS D'ERREURS EN THERMIQUE LINEAIRE
      WRITE(IFI,111)' SD EVOL_THER    ',RESUCO
      WRITE(IFI,110)' NUMERO D''ORDRE  ',IORDR
      WRITE(IFI,109)' INSTANT         ',INST
      WRITE(IFI,*)'ERREUR             ABSOLUE   /  RELATIVE '//
     &      '/ NORMALISATION'
      WRITE(IFI,108)' TOTAL           ',ERR0,NU0,'%',NORS
      IF (NIVEAU.EQ.2) THEN
        WRITE(IFI,108)' TERME VOLUMIQUE ',TERMVO,TERMV2,'%',TERMV1
        WRITE(IFI,108)' TERME SAUT      ',TERMSA,TERMS2,'%',TERMS1
        WRITE(IFI,108)' TERME FLUX      ',TERMFL,TERMF2,'%',TERMF1
        WRITE(IFI,108)' TERME ECHANGE   ',TERMEC,TERME2,'%',TERME1
      ENDIF
108   FORMAT(A17,D16.8,1X,D16.8,A2,1X,D16.8)
109   FORMAT(A17,D16.8)
110   FORMAT(A17,I5)
111   FORMAT(A17,A8)
      WRITE(IFI,*)
      WRITE(IFI,*) '**********************************************'
      CALL JEDEMA()
      END
