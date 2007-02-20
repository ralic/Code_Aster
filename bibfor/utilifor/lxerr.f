      SUBROUTINE LXERR( VAR , TXT )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     VAR , TXT
C     -----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 20/02/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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
C     SOUS PROGRAMME ERREUR DE L'ANALYSEUR LEXICAL
C     -----------------------------------------------------------------
C IN  VAR : OBJET INCRIMINE
C IN  TXT : MESSAGE D'ERREUR
C     -----------------------------------------------------------------
C     ROUTINE(S) UTILISEE(S) :
C         -
C     ROUTINE(S) FORTRAN     :
C         LEN    WRITE
C     ------------------------------------------------------------------
C FIN LXERR
C     ------------------------------------------------------------------
      CHARACTER*80 TAMPON
      CHARACTER*24 VALK(2)
      INTEGER            ILECT, IECR, LRECL, IEOF, ICOL, ILIG
      COMMON /LXCN02/    ILECT, IECR, LRECL, IEOF, ICOL, ILIG
C
      IF (IECR.GT.0) THEN
         IV = LEN(VAR)
         TAMPON = VAR
         VALK(1) = TAMPON(:IV)
         VALK(2) = TXT
         CALL U2MESG('E', 'UTILIFOR_9',2,VALK,0,0,0,0.D0)
C-DEL    WRITE(IECR,'(5A)') ' <E> > ERREUR LEXICALE (01):  "',
C-DEL+                                            VAR(:IV),'" ',TXT(:IT)
      ENDIF
      END
