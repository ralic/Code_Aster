      SUBROUTINE AVDOMT ( NBVEC, NBORDR, NCYCL, DOMEL, DOMTOT )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 09/05/2011   AUTEUR TRAN V-X.TRAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT   NONE
      INTEGER    NBVEC, NBORDR, NCYCL(NBVEC)
      REAL*8     DOMEL(NBVEC*NBORDR), DOMTOT(NBVEC)
C ----------------------------------------------------------------------
C BUT: CALCULER LE DOMMAGE TOTAL (CUMUL) POUR TOUS LES VECTEURS NORMAUX.
C ----------------------------------------------------------------------
C ARGUMENTS :
C  NBVEC    IN   I  : NOMBRE DE VECTEURS NORMAUX.
C  NBORDR   IN   I  : NOMBRE DE NUMEROS D'ORDRE.
C  NCYCL    IN   I  : NOMBRE DE CYCLES ELEMENTAIRES POUR TOUS LES
C                     VECTEURS NORMAUX.
C  DOMEL    IN   R  : VECTEUR CONTENANT LES VALEURS DES DOMMAGES
C                     ELEMENTAIRES, POUR TOUS LES SOUS CYCLES
C                     DE CHAQUE VECTEUR NORMAL.
C  DOMTOT   OUT  R  : VECTEUR CONTENANT LES DOMMAGES TOTAUX (CUMUL)
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
      INTEGER       IVECT, ICYCL, ADRS, I
C     ------------------------------------------------------------------
C234567                                                              012
C
      CALL JEMARQ()
C
C INITIALISATION

       DO 100 I = 1,NBVEC    
         DOMTOT(I) = 0
100   CONTINUE 

      DO 10 IVECT=1, NBVEC
         DO 20 ICYCL=1, NCYCL(IVECT)
            ADRS = (IVECT-1)*NBORDR + ICYCL
            DOMTOT(IVECT) = DOMTOT(IVECT) + DOMEL(ADRS)
 20      CONTINUE
 10   CONTINUE

      CALL JEDEMA()
C
      END
