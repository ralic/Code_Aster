      SUBROUTINE AVCRIT( NBVEC, NBORDR, VALA, COEFPA, NCYCL, VMIN,
     &                   VMAX, OMIN, OMAX, NOMCRI, VSIGN, VPHYDR,
     &                   SIGEQ )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 10/01/2005   AUTEUR F1BHHAJ J.ANGLES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE F1BHHAJ J.ANGLES
      IMPLICIT      NONE
      INTEGER       NBVEC, NBORDR, NCYCL(NBVEC)
      INTEGER       OMIN(NBVEC*(NBORDR+2)), OMAX(NBVEC*(NBORDR+2))
      REAL*8        VALA, COEFPA
      REAL*8        VMIN(NBVEC*(NBORDR+2)), VMAX(NBVEC*(NBORDR+2))
      REAL*8        VSIGN(NBVEC*NBORDR), VPHYDR(NBORDR)
      REAL*8        SIGEQ(NBVEC*NBORDR)
      CHARACTER*16  NOMCRI
C ----------------------------------------------------------------------
C BUT: CALCULER LA CONTRAINTE EQUIVALENTE POUR TOUS LES VECTEURS NORMAUX
C      A TOUS LES NUMEROS D'ORDRE.
C ----------------------------------------------------------------------
C ARGUMENTS :
C  NBVEC    IN   I  : NOMBRE DE VECTEURS NORMAUX.
C  NBORDR   IN   I  : NOMBRE DE NUMEROS D'ORDRE.
C  VALA     IN   R  : VALEUR DU PARAMETRE a ASSOCIE AU CRITERE.
C  COEFPA   IN   R  : COEFFICIENT DE PASSAGE CISAILLEMENT - UNIAXIAL.
C  NCYCL    IN   I  : NOMBRE DE CYCLES ELEMENTAIRES POUR TOUS LES
C                     VECTEURS NORMAUX.
C  VMIN     IN   R  : VALEURS MIN DES CYCLES ELEMENTAIRES POUR TOUS LES
C                     VECTEURS NORMAUX.
C  VMAX     IN   R  : VALEURS MAX DES CYCLES ELEMENTAIRES POUR TOUS LES
C                     VECTEURS NORMAUX.
C  OMIN     IN   I  : NUMEROS D'ORDRE ASSOCIES AUX VALEURS MIN DES
C                     CYCLES ELEMENTAIRES POUR TOUS LES VECTEURS
C                     NORMAUX.
C  OMAX     IN   I  : NUMEROS D'ORDRE ASSOCIES AUX VALEURS MAX DES
C                     CYCLES ELEMENTAIRES POUR TOUS LES VECTEURS
C                     NORMAUX.
C  VSIGN    IN   R  : VECTEUR CONTENANT LES VALEURS DE LA CONTRAINTE
C                     NORMALE, POUR TOUS LES NUMEROS D'ORDRE
C                     DE CHAQUE VECTEUR NORMAL, ON UTILISE
C                     VSIGN UNIQUEMENT DANS LE CRITERE DOMM_MAXI.
C  VPHYDR   IN   R  : VECTEUR CONTENANT LA PRESSION HYDROSTATIQUE A
C                     TOUS LES INSTANTS, ON UTILISE VPHYDR
C                     UNIQUEMENT DANS LE CRITERE DE DANG VAN.
C  SIGEQ    OUT  R  : VECTEUR CONTENANT LES VALEURS DE LA CONTRAINTE
C                     EQUIVALENTE, POUR TOUS LES NUMEROS D'ORDRE
C                     DE CHAQUE VECTEUR NORMAL.
C ----------------------------------------------------------------------
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
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ------------------------------------------------------------------
      INTEGER    I, IVECT, AD0, AD1, AD2, ICYCL, NVAL
      REAL*8     COEPRE
C     ------------------------------------------------------------------
C
C234567                                                              012

      CALL JEMARQ()
C
      CALL GETVR8(' ','COEF_PREECROU',1,1,1,COEPRE,NVAL)

C 1. CRITERE DOMM_MAXI (MATAKE MODIFIE AMPLITUDE VARIABLE)

      IF (NOMCRI(1:9) .EQ. 'DOMM_MAXI') THEN
         DO 10 IVECT=1, NBVEC
            AD0 = (IVECT-1)*NBORDR
            DO 20 ICYCL=1, NCYCL(IVECT)
               AD1 = (IVECT-1)*NBORDR + ICYCL
               AD2 = (IVECT-1)*(NBORDR+2) + ICYCL
               SIGEQ(AD1)= COEPRE*ABS((VMAX(AD2) - VMIN(AD2))/2.0D0) +
     &                       VALA*MAX(VSIGN(AD0+OMAX(AD2)),
     &                                VSIGN(AD0+OMIN(AD2)),0.0D0)
               SIGEQ(AD1)= SIGEQ(AD1)*COEFPA

 20         CONTINUE
 10      CONTINUE

C 2. CRITERE DE DANG_VAN MODIFIE (AMPLITUDE VARIABLE)

      ELSEIF (NOMCRI(1:16) .EQ. 'DANG_VAN_MODI_AV') THEN
         DO 30 IVECT=1, NBVEC
            AD0 = (IVECT-1)*NBORDR
            DO 40 ICYCL=1, NCYCL(IVECT)
               AD1 = (IVECT-1)*NBORDR + ICYCL
               AD2 = (IVECT-1)*(NBORDR+2) + ICYCL
               SIGEQ(AD1)= COEPRE*ABS((VMAX(AD2) - VMIN(AD2))/2.0D0) +
     &                       VALA*MAX(VPHYDR(OMAX(AD2)),
     &                                VPHYDR(OMIN(AD2)),0.0D0)
               SIGEQ(AD1)= SIGEQ(AD1)*COEFPA

 40         CONTINUE
 30      CONTINUE

      ENDIF

      CALL JEDEMA()
C
      END
