subroutine lireco(motfac, noma, nomo, izone, listme,&
                  listmm, listne, listnm, nbmaes, nbnoes,&
                  nbmama, nbnoma)
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
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/reliem.h'
    character(len=16) :: motfac
    character(len=8) :: noma, nomo
    integer :: izone
    character(len=24) :: listme, listmm
    character(len=24) :: listne, listnm
    integer :: nbmaes, nbnoes
    integer :: nbmama, nbnoma
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - LECTURE DONNEES)
!
! LECTURE DES SURFACES DE CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  MOTFAC : MOT-CLE FACTEUR (VALANT 'CONTACT')
! IN  NOMA   : NOM DU MAILLAGE
! IN  NOMO   : NOM DU MODELE
! IN  IZONE  : INDICE DE LA ZONE DE CONTACT
! I/O LISTME : NOM DE LA LISTE CONTENANT LES MAILLES ESCLAVES
! I/O LISTMM : NOM DE LA LISTE CONTENANT LES MAILLES MAITRES
! I/O LISTNE : NOM DE LA LISTE CONTENANT LES NOEUDS ESCLAVES
! I/O LISTNM : NOM DE LA LISTE CONTENANT LES NOEUDS MAITRES
! OUT NBMAES : NOMBRE DE MAILLES ESCLAVES
! OUT NBMAMA : NOMBRE DE MAILLES MAITRES
! OUT NBMAES : NOMBRE DE MAILLES ESCLAVES
! OUT NBMAMA : NOMBRE DE MAILLES MAITRES
!
!
!
!
!
    integer :: nbmocl
    character(len=8) :: k8bla
    character(len=16) :: limocl(2), tymocl(2)
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nbmocl = 2
    nbmaes = 0
    nbmama = 0
    nbnoes = 0
    nbnoma = 0
    k8bla = ' '
!
! --- MAILLES
!
    tymocl(1) = 'GROUP_MA'
    tymocl(2) = 'MAILLE'
!
! --- MAILLES ESCLAVES
!
    limocl(1) = 'GROUP_MA_ESCL'
    limocl(2) = 'MAILLE_ESCL'
    call reliem(k8bla, noma, 'NU_MAILLE', motfac, izone,&
                nbmocl, limocl, tymocl, listme, nbmaes)
!
! --- MAILLES MAITRES
!
    limocl(1) = 'GROUP_MA_MAIT'
    limocl(2) = 'MAILLE_MAIT'
    call reliem(k8bla, noma, 'NU_MAILLE', motfac, izone,&
                nbmocl, limocl, tymocl, listmm, nbmama)
!
! --- NOEUDS ESCLAVES
!
    limocl(1) = 'GROUP_MA_ESCL'
    limocl(2) = 'MAILLE_ESCL'
    call reliem(nomo, noma, 'NU_NOEUD', motfac, izone,&
                nbmocl, limocl, tymocl, listne, nbnoes)
!
! --- NOEUDS MAITRES
!
    limocl(1) = 'GROUP_MA_MAIT'
    limocl(2) = 'MAILLE_MAIT'
    call reliem(nomo, noma, 'NU_NOEUD', motfac, izone,&
                nbmocl, limocl, tymocl, listnm, nbnoma)
!
! --- MENAGES
!
    call jedema()
end subroutine
