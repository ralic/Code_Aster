      SUBROUTINE GCHARG ( MODELE, NCHAR, LCHAR, CHVOLU, CF1D2D, CF2D3D,
     +                    CHPRES, CHEPSI, CHPESA, CHROTA, FONC, EPSI )
      IMPLICIT NONE
      INTEGER         NCHAR
      CHARACTER*8     MODELE, LCHAR(*)
      CHARACTER*24    CHVOLU,CF1D2D,CF2D3D,CHPRES,CHEPSI,CHPESA,CHROTA
      LOGICAL         FONC, EPSI
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 12/04/99   AUTEUR CIBHHLV L.VIVAN 
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
C
C ----------------------------------------------------------------------
C
      INTEGER       IBID, IER, I, IF3D3D, IF2D2D, IF1D2D, IF2D3D,
     +              IPRESS, IEPSIN, IROTA, IPESA, IRET
      CHARACTER*8   K8B
      CHARACTER*16  TYPE, OPER
      CHARACTER*24  BLANC
C     ------------------------------------------------------------------
C
      CALL GETRES ( K8B, TYPE, OPER )
      FONC   = .FALSE.
      EPSI   = .FALSE.
      BLANC = '                        '
      CHVOLU = BLANC
      CF1D2D = BLANC
      CF2D3D = BLANC
      CHPRES = BLANC
      CHEPSI = BLANC
      CHPESA = BLANC
      CHROTA = BLANC
C
      IER = 0
      DO 10 I = 1 , NCHAR
C
         CALL DISMOI('F','TYPE_CHARGE',LCHAR(I),'CHARGE',IBID,K8B,IRET)
         IF ( K8B(5:7) .EQ. '_FO' )  FONC = .TRUE.
C
         CALL JEEXIN ( LCHAR(I)//'.CHME.F3D3D.DESC', IF3D3D )
         IF ( IF3D3D .NE. 0 ) THEN
            IF ( CHVOLU .EQ. BLANC ) THEN
               CHVOLU = LCHAR(I)//'.CHME.F3D3D.DESC'
            ELSE
               IER = IER + 1
               CALL UTDEBM('E',OPER,'IL FAUT DONNER 1 SEUL CHARGEMENT')
               CALL UTIMPK('S',' DE TYPE ',1,'FORCE VOLUMIQUE')
               CALL UTIMPK('L','  CHARGES FOURNIES: ',1,CHVOLU(1:8))
               CALL UTIMPK('S','  ET ',1,LCHAR(I)(1:8))
               CALL UTFINM()
            ENDIF
         ENDIF
C
         CALL JEEXIN ( LCHAR(I)//'.CHME.F2D2D.DESC', IF2D2D )
         IF ( IF2D2D .NE. 0 ) THEN
            IF ( CHVOLU .EQ. BLANC ) THEN
               CHVOLU = LCHAR(I)//'.CHME.F2D2D.DESC'
            ELSE
               IER = IER + 1
               CALL UTDEBM('E',OPER,'IL FAUT DONNER 1 SEUL CHARGEMENT')
               CALL UTIMPK('S',' DE TYPE ',1,'FORCE VOLUMIQUE')
               CALL UTIMPK('L','  CHARGES FOURNIES: ',1,CHVOLU(1:8))
               CALL UTIMPK('S','  ET ',1,LCHAR(I)(1:8))
               CALL UTFINM()
            ENDIF
         ENDIF
C
         CALL JEEXIN ( LCHAR(I)//'.CHME.F1D2D.DESC', IF1D2D )
         IF ( IF1D2D .NE. 0 ) THEN
            IF ( CF1D2D .EQ. BLANC ) THEN
               CF1D2D = LCHAR(I)//'.CHME.F1D2D.DESC'
            ELSE
               IER = IER + 1
               CALL UTDEBM('E',OPER,'IL FAUT DONNER 1 SEUL CHARGEMENT')
               CALL UTIMPK('S',' DE TYPE ',1,'FORCE_...')
               CALL UTIMPK('L','  CHARGES FOURNIES: ',1,CF1D2D(1:8))
               CALL UTIMPK('S','  ET ',1,LCHAR(I)(1:8))
               CALL UTFINM()
            ENDIF
         ENDIF
C
         CALL JEEXIN ( LCHAR(I)//'.CHME.F2D3D.DESC', IF2D3D )
         IF ( IF2D3D .NE. 0 ) THEN
            IF ( CF2D3D .EQ. BLANC ) THEN
               CF2D3D = LCHAR(I)//'.CHME.F2D3D.DESC'
            ELSE
               IER = IER + 1
               CALL UTDEBM('E',OPER,'IL FAUT DONNER 1 SEUL CHARGEMENT')
               CALL UTIMPK('S',' DE TYPE ',1,'FORCE_...')
               CALL UTIMPK('L','  CHARGES FOURNIES: ',1,CF2D3D(1:8))
               CALL UTIMPK('S','  ET ',1,LCHAR(I)(1:8))
               CALL UTFINM()
            ENDIF
         ENDIF
C
         CALL JEEXIN ( LCHAR(I)//'.CHME.PRESS.DESC', IPRESS )
         IF ( IPRESS .NE. 0 ) THEN
            IF ( CHPRES .EQ. BLANC ) THEN
               CHPRES = LCHAR(I)//'.CHME.PRESS.DESC'
            ELSE
               IER = IER + 1
               CALL UTDEBM('E',OPER,'IL FAUT DONNER 1 SEUL CHARGEMENT')
               CALL UTIMPK('S',' DE TYPE ',1,'PRESSION')
               CALL UTIMPK('L','  CHARGES FOURNIES: ',1,CHPRES(1:8))
               CALL UTIMPK('S','  ET ',1,LCHAR(I)(1:8))
               CALL UTFINM()
            ENDIF
         ENDIF
C
         CALL JEEXIN ( LCHAR(I)//'.CHME.EPSIN.DESC', IEPSIN )
         IF ( IEPSIN .NE. 0 ) THEN
            EPSI = .TRUE.
            IF ( CHEPSI .EQ. BLANC ) THEN
               CHEPSI = LCHAR(I)//'.CHME.EPSIN.DESC'
            ELSE
               IER = IER + 1
               CALL UTDEBM('E',OPER,'IL FAUT DONNER 1 SEUL CHARGEMENT')
               CALL UTIMPK('S',' DE TYPE ',1,'EPSI_INIT')
               CALL UTIMPK('L','  CHARGES FOURNIES: ',1,CHEPSI(1:8))
               CALL UTIMPK('S','  ET ',1,LCHAR(I)(1:8))
               CALL UTFINM()
            ENDIF
         ENDIF
C
         CALL JEEXIN ( LCHAR(I)//'.CHME.PESAN.DESC', IPESA )
         IF ( IPESA .NE. 0 ) THEN
            IF ( CHPESA .EQ. BLANC ) THEN
               CHPESA = LCHAR(I)//'.CHME.PESAN.DESC'
            ELSE
               IER = IER + 1
               CALL UTDEBM('E',OPER,'IL FAUT DONNER 1 SEUL CHARGEMENT')
               CALL UTIMPK('S',' DE TYPE ',1,'PESANTEUR')
               CALL UTIMPK('L','  CHARGES FOURNIES: ',1,CHPESA(1:8))
               CALL UTIMPK('S','  ET ',1,LCHAR(I)(1:8))
               CALL UTFINM()
            ENDIF
         ENDIF
C
         CALL JEEXIN ( LCHAR(I)//'.CHME.ROTAT.DESC', IROTA )
         IF ( IROTA .NE. 0 ) THEN
            IF ( CHROTA .EQ. BLANC ) THEN
               CHROTA = LCHAR(I)//'.CHME.ROTAT.DESC'
            ELSE
               IER = IER + 1
               CALL UTDEBM('E',OPER,'IL FAUT DONNER 1 SEUL CHARGEMENT')
               CALL UTIMPK('S',' DE TYPE ',1,'ROTATION')
               CALL UTIMPK('L','  CHARGES FOURNIES: ',1,CHROTA(1:8))
               CALL UTIMPK('S','  ET ',1,LCHAR(I)(1:8))
               CALL UTFINM()
            ENDIF
         ENDIF
C
 10   CONTINUE
C
      IF ( IER .NE. 0 ) THEN
         CALL UTMESS('F',OPER,'******* ERREUR DONNEES *******')
      ENDIF
C
C  -  SI ABSENCE D'UN CHAMP DE FORCES, CREATION D'UN CHAMP NUL
C
      IF ( CHVOLU .EQ. BLANC ) THEN
         CALL MEFOR0 ( MODELE, CHVOLU, FONC )
      ENDIF
      IF ( CF1D2D .EQ. BLANC ) THEN
         CALL MEFOR1 ( MODELE, CF1D2D, FONC )
      ENDIF
      IF ( CF2D3D .EQ. BLANC ) THEN
         CALL MEFOR0 ( MODELE, CF2D3D, FONC )
      ENDIF
      IF ( CHPRES .EQ. BLANC ) THEN
         CALL MEPRES ( MODELE, CHPRES, FONC )
      ENDIF
C
      END
