      SUBROUTINE CVGCNT(ITEMAX,NEQ,DEPDEL,AUTOC1,AUTOC2,VECONT,LREAC)

      IMPLICIT      NONE
      LOGICAL ITEMAX,LREAC(4)
      INTEGER NEQ,VECONT(2)
      CHARACTER*19 AUTOC1,AUTOC2
      CHARACTER*24 DEPDEL
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 27/05/2003   AUTEUR PABHHHH N.TARDIEU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C BUT : CONVERGENCE LIEE A LA REACTUALISATION GEOMETRIQUE DU CONTACT ---
C ======================================================================
C IN     : ITEMAX : .TRUE. SI ITERATION MAXIMUM ATTEINTE                
C IN/OUT : VECONT : (1) = NOMBRE DE REACTUALISATION GEOMETRIQUE    
C        :        :       A EFFECTUER / -1 SI AUTOMATIQUE             
C        :        :                   /  0 SI PAS DE REACTUALISATION  
C        :        :                   /  N REACTUALISATIONS  
C        :        : (2) = NOMBRE DE REACTUALISATIONS GEOMETRIQUES    
C                         EFFECTUEES
C OUT    : LREAC  : (1) = TRUE  SI REACTUALISATION A FAIRE  
C        :        : (2) = TRUE  SI ATTENTE POINT FIXE CONTACT
C        :        : (3) = TRUE  SI METHODE CONTINUE
C        :        : (4) = TRUE  SI MODELISATION DU CONTACT
C ======================================================================
C --------------- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------
      CHARACTER*32 JEXNUM,JEXNOM
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
      INTEGER JDEPDE,JAUTO1,JAUTO2,II,IRET,JMAXDE
      REAL*8 AUTONO,TEMP1,TEMP2,R8PREM,TOLERA,RMIN
      CHARACTER*8  MAXDEP

      CALL JEMARQ()

      LREAC(1) = .TRUE.
      LREAC(3) = .TRUE.
C --- CONVERGENCE GEOMETRIQUE MAIS ATTENTE POINT FIXE CONTACT
C     =======================================================
      IF (LREAC(2)) THEN
        CALL NMIMPR('IMPR','CONV_OK',' ',0.D0,0)
        CALL NMIMPR('IMPR','FIXE_NON',' ',0.D0,0)
        LREAC(1) = .FALSE.
        LREAC(3) = .FALSE.
      ELSE
        VECONT(2) = VECONT(2) + 1
C --- CORRESPOND A REAC_GEOM = AUTOMATIQUE
C     =====================================
        IF (VECONT(1).LT.0) THEN
          TOLERA = 5.0D-2
          TEMP1 = 0.D0
          TEMP2 = 0.D0
          CALL JEVEUO(DEPDEL(1:19)//'.VALE','L',JDEPDE)
          CALL JEVEUO(AUTOC1//'.VALE','E',JAUTO1)
          CALL JEVEUO(AUTOC2//'.VALE','E',JAUTO2)
          DO 10 II = 1,NEQ
            ZR(JAUTO2-1+II) = ZR(JAUTO2-1+II) + ZR(JAUTO1-1+II)
            ZR(JAUTO1-1+II) = ZR(JDEPDE-1+II) - ZR(JAUTO2-1+II)
   10     CONTINUE
C  ---  CALCUL DU MAX DE LA NORME DU DEPL
          CALL CNOMAX(AUTOC1,TEMP1)
          CALL CNOMAX(AUTOC2,TEMP2)
          MAXDEP='&&CVGCNT'
          CALL JEEXIN(MAXDEP,IRET)
C  ---  STOCKAGE DU MAX DE LA NORME DU DEPL
          IF (IRET.EQ.0) THEN
             CALL WKVECT(MAXDEP,'V V R',1,JMAXDE)
             ZR(JMAXDE-1+1)=TEMP2
             RMIN=R8PREM()
          ELSE
             CALL JEVEUO(MAXDEP,'E',JMAXDE)
             ZR(JMAXDE-1+1)=MAX(ZR(JMAXDE-1+1),TEMP2)
             RMIN=1.D-6*ZR(JMAXDE-1+1)
          ENDIF
          IF (TEMP2.LT.RMIN) THEN
            IF (TEMP2.EQ.0.D0) THEN
              AUTONO = 10.0D0*TOLERA
            ELSE
              AUTONO = 1.D-1*TOLERA
            END IF
          ELSE
            AUTONO = TEMP1/TEMP2
          END IF
          IF (AUTONO.LT.TOLERA) THEN
            CALL NMIMPR('IMPR','CONV_OK',' ',0.D0,0)
            CALL NMIMPR('IMPR','AUTO_GEO',' ',0.D0,VECONT(2))
          ELSE
            LREAC(3) = .FALSE.
          END IF
C --- CORRESPOND A REAC_GEOM = SANS
C     =============================
        ELSE IF (VECONT(1).EQ.0) THEN
          CALL NMIMPR('IMPR','CONV_OK',' ',0.D0,0)
          CALL NMIMPR('IMPR','GEOM_MIN',' ',0.D0,0)
          LREAC(1) = .FALSE.
        ELSE
C --- CORRESPOND A REAC_GEOM = CONTROLE
C     =================================
          IF (VECONT(2).EQ.VECONT(1)) THEN
            CALL NMIMPR('IMPR','CONV_OK',' ',0.D0,0)
            CALL NMIMPR('IMPR','AUTO_GEO',' ',0.D0,VECONT(1))
          ELSE
            LREAC(3) = .FALSE.
          END IF
        END IF
      END IF
      IF (.NOT.LREAC(3)) THEN
        IF (ITEMAX) THEN
          CALL NMIMPR('IMPR','ITER_MAXI',' ',0.D0,0)
        END IF
      END IF

      CALL JEDEMA()

      END
