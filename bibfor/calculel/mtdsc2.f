         SUBROUTINE MTDSC2(MATAS,OBJET,EOUL,ADRESS)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 01/02/2000   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
         IMPLICIT NONE
         CHARACTER*(*) MATAS,OBJET,EOUL
         INTEGER ADRESS

C -----------------------------------------------------------
C   BUT : RECUPERER L'ADRESSE D'UN OBJET D'UNE MATR_ASSE

C   MATAS K19  IN/JXIN : NOM DE LA MATR_ASSE
C   OBJET K4   IN      : NOM D'UN SUFFIXE : ABLO,ADIA,...
C   EOUL  K1   IN      : 'E' : EN ECRITURE
C                        'L' : EN LECTURE
C   ADRESS I   OUT     : ADRESSE DANS ZI, ZR, ... DE L'OBJET


C  ATTENTION : CETTE ROUTINE NE FAIT PAS JEMARQ/JEDEMA
C              POUR NE PAS INVALIDER L'ADRESSE "OUT"
C -----------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
        CHARACTER*19 MAT
        CHARACTER*14 NU
        CHARACTER*4 OBJ
        INTEGER I1,I2,JREFA


        MAT=MATAS
        OBJ=OBJET

        IF (OBJ.EQ.'REFA') THEN
          CALL JEVEUO(MAT//'.'//OBJ,EOUL,ADRESS)
        ELSE IF (OBJ.EQ.'CONL') THEN
          CALL JEVEUO(MAT//'.'//OBJ,EOUL,ADRESS)
        ELSE IF (OBJ.EQ.'COND') THEN
          CALL JEVEUO(MAT//'.'//OBJ,EOUL,ADRESS)
        ELSE IF (OBJ.EQ.'ABLI') THEN
          CALL JEVEUO(MAT//'.'//OBJ,EOUL,ADRESS)
        ELSE IF (OBJ.EQ.'ALIG') THEN
          CALL JEVEUO(MAT//'.'//OBJ,EOUL,ADRESS)
        ELSE IF (OBJ.EQ.'LLIG') THEN
          CALL JEVEUO(MAT//'.'//OBJ,EOUL,ADRESS)
        ELSE IF (OBJ.EQ.'CONI') THEN
          CALL JEVEUO(MAT//'.'//OBJ,EOUL,ADRESS)


        ELSE IF ((OBJ.EQ.'ADIA').OR.(OBJ.EQ.'ABLO')) THEN
C       --------------------------------------------------
          CALL JEVEUO(MAT//'.REFA',EOUL,JREFA)
          NU=ZK24(JREFA-1+2)
          CALL JEEXIN(NU//'.SLCS.ADIA',I1)
          CALL JEEXIN(NU//'.SMOS.ADIA',I2)
          IF (I1.GT.0) THEN
            CALL JEVEUO(NU//'.SLCS.'//OBJ,EOUL,ADRESS)
          ELSE
            IF (I2.EQ.0) CALL UTMESS('F','MTDSC2','STOP')
            CALL JEVEUO(NU//'.SMOS.'//OBJ,EOUL,ADRESS)
          END IF


        ELSE
          CALL UTMESS('F','MTDSC2','OBJET IMPREVU :'//OBJ)
        END IF

        END
