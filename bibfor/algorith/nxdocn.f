      SUBROUTINE NXDOCN (PARCRI,PARCRR)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 04/04/2000   AUTEUR DURAND C.DURAND 
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
      INTEGER            PARCRI(3)
      REAL*8                    PARCRR(2)
C ----------------------------------------------------------------------
C     SAISIE DES CRITERES DE CONVERGENCE
C
C OUT PARCRI  : PARAMETRES ENTIERS DU CRITERE
C               PARCRI(1) = 1 TEST EN ABSOLU  SUR LE RESIDU
C               PARCRI(2) = 1 TEST EN RELATIF SUR LE RESIDU
C               PARCRI(3) = NB MAXIMUM D'ITERATION
C OUT PARCRR  : PARAMETRES REELS DU CRITERE
C
C ----------------------------------------------------------------------
      CHARACTER*8       K8BID
      CHARACTER*16      NOMCVG
      INTEGER           N1,IOCC
C ----------------------------------------------------------------------
C --- RECUPERATION DES CRITERES DE CONVERGENCE
C
      NOMCVG = 'CONVERGENCE'
      CALL GETFAC(NOMCVG,IOCC)
      IF ( IOCC .EQ. 1 ) THEN
        CALL GETVR8(NOMCVG,'RESI_GLOB_MAXI',1,1,1,PARCRR(1),
     &                                                   PARCRI(1))
        CALL GETVR8(NOMCVG,'RESI_GLOB_RELA',1,1,1,PARCRR(2),
     &                                                   PARCRI(2))
        IF ( PARCRI(1)+PARCRI(2).EQ.0 ) THEN
          PARCRI(2) = 1
          PARCRR(2) = 1.D-6
        ENDIF
C
        CALL GETVIS(NOMCVG,'ITER_GLOB_MAXI',1,1,1,PARCRI(3),N1)
      ENDIF
C FIN ------------------------------------------------------------------
      END
