      SUBROUTINE TBEXIP ( NOMTA,PARA,EXIST,TYPPAR)
      IMPLICIT   NONE
      CHARACTER*(*)       NOMTA, PARA,TYPPAR
      LOGICAL             EXIST
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 27/09/2004   AUTEUR CIBHHLV L.VIVAN 
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
C      EXISTENCE D'UN PARAMETRE DANS UNE TABLE.
C ----------------------------------------------------------------------
C IN  : NOMTA  : NOM DE LA STRUCTURE "TABLE".
C IN  : PARA   : PARAMETRE A CHERCHER
C OUT : EXIST  : LE PARAMETRE EXISTE OU N'EXISTE PAS
C OUT : TYPPAR : TYPE DU PARAMETRE (S'IL EXISTE) : I/R/C/K8/K16,...
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
C ----------------------------------------------------------------------
      INTEGER      IRET, NBPARA, JTBNP, JTBLP, IPAR
      CHARACTER*19 NOMTAB
      CHARACTER*24 INPAR, JNPAR
C DEB------------------------------------------------------------------
C
      CALL JEMARQ ( )
C
      NOMTAB = NOMTA
      INPAR  = PARA
      EXIST  = .FALSE.
      TYPPAR = '????'
C
C     --- VERIFICATION DE LA TABLE ---
C
      CALL JEEXIN ( NOMTAB//'.TBBA', IRET )
      IF ( IRET .EQ. 0 ) THEN
         CALL UTMESS('F','TBEXIP','LA TABLE N''EXISTE PAS')
      ENDIF
C
      CALL JEVEUO ( NOMTAB//'.TBNP' , 'L', JTBNP )
      NBPARA = ZI(JTBNP  )
      IF ( NBPARA .EQ. 0 ) THEN
         CALL UTMESS('F','TBEXIP','PAS DE PARAMETRES DEFINIS')
      ENDIF
C
C     --- VERIFICATION QUE LE PARAMETRE EXISTE DANS LA TABLE ---
C
      CALL JEVEUO ( NOMTAB//'.TBLP' , 'L', JTBLP )
      DO 10 IPAR = 1 , NBPARA
         JNPAR = ZK24(JTBLP+4*(IPAR-1))
         IF ( INPAR .EQ. JNPAR ) THEN
            EXIST = .TRUE.
            TYPPAR = ZK24(JTBLP-1+4*(IPAR-1)+2)
            GOTO 12
         ENDIF
 10   CONTINUE
 12   CONTINUE
C
      CALL JEDEMA()
      END
