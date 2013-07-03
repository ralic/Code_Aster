subroutine nbzoco(motfac, noma, izone, jzone, nsuco)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
#include "jeveux.h"
#include "asterfort/getvem.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    character(len=8) :: noma
    character(len=16) :: motfac
    integer :: izone
    integer :: jzone
    integer :: nsuco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - LECTURE DONNEES)
!
! DETERMINATION DU NOMBRE DE SURFACES DE CONTACT
! REMPLISSAGE DU POINTEUR ASSOCIE JZONE
!
! ----------------------------------------------------------------------
!
!
! IN  MOTFAC : MOT-CLE FACTEUR (VALANT 'CONTACT')
! IN  NOMA   : NOM DU MAILLAGE
! IN  IZONE  : INDICE POUR LIRE LES DONNEES DANS AFFE_CHAR_MECA
! IN  JZONE  : POINTEUR DES ZONES DE CONTACT
! I/O NSUCO  : REPERE DU NOMBRE DE SURFACES DE CONTACT POUR CETTE
!              ZONE DE CONTACT
!
!
!
!
    character(len=8) :: k8bid
    integer :: ngmait, ngescl, nmmait, nmescl
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    ngmait = 0
    ngescl = 0
    nmmait = 0
    nmescl = 0
!
! --- DETERMINATION DU NOMBRE TOTAL DE SURFACES
!
    call getvem(noma, 'GROUP_MA', motfac, 'GROUP_MA_MAIT', izone,&
                iarg, 0, k8bid, ngmait)
    call getvem(noma, 'GROUP_MA', motfac, 'GROUP_MA_ESCL', izone,&
                iarg, 0, k8bid, ngescl)
    call getvem(noma, 'MAILLE', motfac, 'MAILLE_MAIT', izone,&
                iarg, 0, k8bid, nmmait)
    call getvem(noma, 'MAILLE', motfac, 'MAILLE_ESCL', izone,&
                iarg, 0, k8bid, nmescl)
!
! --- NOMBRE TOTAL DE SURFACES DE CONTACT
!
    if (ngmait .ne. 0) nsuco = nsuco + 1
    if (ngescl .ne. 0) nsuco = nsuco + 1
    if (nmmait .ne. 0) nsuco = nsuco + 1
    if (nmescl .ne. 0) nsuco = nsuco + 1
!
! --- MISE A JOUR DU POINTEUR SUR LES SURFACES DE CONTACT DE LA ZONE
!
    zi(jzone+izone) = nsuco
!
    call jedema()
end subroutine
