      SUBROUTINE OP0050()
      IMPLICIT  NONE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 30/06/2010   AUTEUR DELMAS J.DELMAS 
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
C          OPERATEUR INTE_MAIL_2D
C
C ----------------------------------------------------------------------
      INTEGER       IBID, NBPARM, NBPARS, NBPARA, N1
      CHARACTER*8   K8B, NOMAIL
C----------------------------------------------------------------------
C
C----------------------------------------------------------------------
C
      CALL INFMAJ
C----------------------------------------------------------------------
C
C                 LE MAILLAGE DOIT ETRE 2D OU PLAN
C
C----------------------------------------------------------------------
      CALL GETVID ( ' ', 'MAILLAGE' , 0,1,1, NOMAIL, N1 )
      CALL DISMOI ('F', 'Z_CST', NOMAIL,'MAILLAGE', IBID, K8B, IBID )
      IF ( K8B(1:3) .EQ. 'NON' ) THEN
         CALL U2MESS('F','INTEMAIL_10')
      ENDIF
C----------------------------------------------------------------------
C
C                     D E F I _ C H E M I N
C
C----------------------------------------------------------------------
      CALL GETFAC ( 'DEFI_CHEMIN'  , NBPARM )
      IF ( NBPARM .GT. 0 ) THEN
         CALL I2CHEM ( NOMAIL , NBPARM )
         GOTO 9999
      ENDIF
C----------------------------------------------------------------------
C
C            D E F I _ A R C   ET   D E F I _ S E G M E N T
C
C----------------------------------------------------------------------
      CALL GETFAC ( 'DEFI_SEGMENT' , NBPARS )
      CALL GETFAC ( 'DEFI_ARC'     , NBPARA )
      IF ( NBPARS .GT. 0  .OR.  NBPARA .GT. 0 ) THEN
         CALL I2SEGM ( NOMAIL , NBPARS , NBPARA )
         GOTO 9999
      ENDIF
C
 9999 CONTINUE
C
      END
