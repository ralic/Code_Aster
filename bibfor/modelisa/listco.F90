subroutine listco(char, motfac, noma, nomo, nzoco,&
                  nmaco, nnoco)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit     none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/liexco.h"
#include "asterfort/nbsuco.h"
    character(len=8) :: char
    character(len=16) :: motfac
    character(len=8) :: noma, nomo
    integer :: nzoco
    integer :: nmaco
    integer :: nnoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - LECTURE DONNEES)
!
! STOCKAGE DES MAILLES ET NOEUDS DE CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  MOTFAC : MOT-CLE FACTEUR (VALANT 'CONTACT')
! IN  NOMA   : NOM DU MAILLAGE
! IN  NOMO   : NOM DU MODELE
! IN  NZOCO  : NOMBRE DE ZONES DE CONTACT
! OUT NMACO  : NOMBRE TOTAL DE MAILLES DES SURFACES DE CONTACT
! OUT NNOCO  : NOMBRE TOTAL DE NOEUDS DES SURFACES DE CONTACT
!
!
!
!
    call jemarq()
!
! --- ON COMPTE LES MAILLES/NOEUDS DES ZONES DE CONTACT
!
    call nbsuco(char, motfac, noma, nomo, nzoco,&
                nmaco, nnoco)
!
! --- ON STOCKE LES MAILLES/NOEUDS DES ZONES DE CONTACT
!
    call liexco(char, motfac, noma, nomo, nzoco,&
                nmaco, nnoco)
!
    call jedema()
end subroutine
