      SUBROUTINE RVRECU ( MCF, IOCC, CHAMP, NOMVEC )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER                  IOCC
      CHARACTER*(*)       MCF,       CHAMP, NOMVEC
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 16/04/96   AUTEUR CIBHHLV L.VIVAN 
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
C     ------------------------------------------------------------------
C IN  IOCC   : INDICE DE L' OCCURENCE
C IN  CHAMP  : NOM DU CHAMP A TRAITER
C     ------------------------------------------------------------------
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
      CHARACTER*32     JEXNUM, JEXNOM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      CHARACTER*1   TYPE
      CHARACTER*8   K8B, FORM
      CHARACTER*19  NCH19
      CHARACTER*24  VECTEU
C
C==================== CORPS DE LA ROUTINE =============================
C
      CALL JEMARQ()
      NCH19 = CHAMP
      VECTEU = NOMVEC
      CALL JELIRA ( NCH19//'.VALE', 'TYPE', IBID, TYPE )
      IF ( TYPE .NE. 'C' ) THEN
         CALL UTMESS('F','RVRECU','ON NE TRAITE QUE LES COMPLEXES')
      ENDIF
      CALL JELIRA ( NCH19//'.VALE', 'LONMAX', NEQ, K8B )
      CALL JEVEUO (NCH19//'.VALE', 'L', JVAL )
      CALL WKVECT ( VECTEU, 'V V R', NEQ, KVAL)
C
      CALL GETVTX ( MCF, 'FORMAT_C', IOCC, 1, 1, FORM, N1 )
C
      IF ( FORM .EQ. 'MODULE' ) THEN
        DO 10 I = 0 , NEQ-1
           A =  DBLE( ZC(JVAL+I) )
           B = DIMAG( ZC(JVAL+I) )
           ZR(KVAL+I) = SQRT( A*A + B*B )
 10     CONTINUE
C
      ELSEIF ( FORM .EQ. 'REEL' ) THEN
        DO 20 I = 0 , NEQ-1
           ZR(KVAL+I) = DBLE( ZC(JVAL+I) )
 20     CONTINUE
C
      ELSEIF ( FORM .EQ. 'IMAG' ) THEN
        DO 30 I = 0 , NEQ-1
           ZR(KVAL+I) = DIMAG( ZC(JVAL+I) )
 30     CONTINUE
C
      ELSE
         CALL UTMESS('F','RVRECU','VRAIMENT DESOLE')
      ENDIF
C
      CALL JEDEMA()
      END
