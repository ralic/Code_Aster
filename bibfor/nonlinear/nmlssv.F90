subroutine nmlssv(mode, lischa, nbsst)
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    implicit none
    include 'jeveux.h'
    include 'asterc/getfac.h'
    include 'asterc/getvid.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/wkvect.h'
    character(len=4) :: mode
    integer :: nbsst
    character(len=19) :: lischa
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL - SOUS-STRUCTURATION)
!
! LECTURE ET PREPARATION POUR SOUS_STRUCT
!
! ----------------------------------------------------------------------
!
!
! IN  MODE   : TYPE d'OPERATION
!              'LECT' LIT LE MOT-CLEF 'SOUS_STRUC'
!              'INIT' CREE LE VECTEUR DES CHARGES POUR SOUS_STRUC
! IN  LISCHA : SD L_CHARGES
! OUT NBSST  : NOMBRE de SOUS-STRUCTURES
!
!
!
!
    character(len=24) :: fomul2
    integer :: jfomu2
    integer :: i, ibid
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    nbsst = 0
    if (mode .eq. 'LECT') then
        call getfac('SOUS_STRUC', nbsst)
    else if (mode.eq.'INIT') then
        call getfac('SOUS_STRUC', nbsst)
        if (nbsst .gt. 0) then
            fomul2 = lischa(1:19)//'.FCSS'
            call wkvect(fomul2, 'V V K24', nbsst, jfomu2)
            do 1 i = 1, nbsst
                call getvid('SOUS_STRUC', 'FONC_MULT', i, iarg, 1,&
                            zk24(jfomu2+i-1), ibid)
                if (ibid .eq. 0) then
                    zk24(jfomu2+i-1) = '&&CONSTA'
                endif
 1          continue
        endif
    else
        call assert(.false.)
    endif
    call jedema()
!
end subroutine
