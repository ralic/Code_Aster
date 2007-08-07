      SUBROUTINE JEDISP ( N , TAB )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 06/08/2007   AUTEUR LEFEBVRE J-P.LEFEBVRE 
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
C TOLE CFT_720 CFT_726 CRP_18 CRS_508
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             N , TAB(*)
C ----------------------------------------------------------------------
C RENVOIE DANS LE TABLEAU TAB LES LONGUEURS MAX DISPONIBLES DANS LA 
C PARTITION MEMOIRE NUMERO 1
C
C IN  N      : TAILLE DU TABLEAU TAB
C IN  TAB    : TAILLE DE SEGMENT DE VALEURS DISPONIBLE
C
C SI L'ALLOCATION DYNAMIQUE EST UTILISEE LA ROUTINE RENVOIE L'ENTIER 
C MAXIMUM DANS LES N VALEURS DU TABLEAU TAB
C
C
C ----------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
C ----------------------------------------------------------------------
      INTEGER          ISTAT
      COMMON /ISTAJE/  ISTAT(4)
      INTEGER          IDINIT   ,IDXAXD   ,ITRECH,ITIAD,ITCOL,LMOTS,IDFR
      COMMON /IXADJE/  IDINIT(2),IDXAXD(2),ITRECH,ITIAD,ITCOL,LMOTS,IDFR
      INTEGER          LDYN , LGDYN
      COMMON /IDYNJE/  LDYN , LGDYN
C ----------------------------------------------------------------------
      LOGICAL          LAMOV
C DEB ------------------------------------------------------------------
      IF ( LDYN .EQ. 1 ) THEN 
        DO 11 K = 1,N
         TAB(K) = ISMAEM()
  11    CONTINUE
        GOTO 200
      ENDIF     
      DO 1 K = 1,N
         TAB(K) = 0
 1    CONTINUE
      IZ = 1
      IF (IDFR .GT .0) IZ=2
      ID  = IDINIT(IZ)
      IDA = ID
      LAMOV  = .FALSE.
 140  CONTINUE
      IS = ISZON ( JISZON + ID )
      IF ( IS .EQ. 0 ) GOTO 125
      ISTA = ISZON(JISZON + IS - 4 )
      IF(  ISZON(JISZON + ID + 3) .EQ. ISTAT(1) .AND.
     &   ( ISTA .EQ. ISTAT(4) .OR. ISTA .EQ. ISTAT(3) .OR.
     &     ISTA .EQ. ISTAT(1) ) ) THEN
         IF ( .NOT. LAMOV ) THEN
             LAMOV = .TRUE.
             IDA   = ID
         ENDIF
         ID  = IS
         GOTO 140
      ENDIF
 125  CONTINUE
      IF ( LAMOV ) THEN
         LAMOV  = .FALSE.
         MAPLAC = ID - IDA - 8
         DO 100 K = 1,N
            IF ( MAPLAC .GT. TAB(K) ) THEN
               DO 101 L = N,K+1,-1
                  TAB(L) = TAB(L-1)
 101           CONTINUE
               TAB(K) = MAPLAC
               GOTO 102
            ENDIF
 100     CONTINUE
 102     CONTINUE
      ENDIF
      ID  = IS
      IF ( IS .NE. 0 ) GOTO 140
 200  CONTINUE     
C FIN-------------------------------------------------------------------
      END
