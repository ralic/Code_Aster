subroutine extmod(basemo, numddl, nume, nbnumo, dmode,&
                  nbeq, nbnoe, iddl, nbddl)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
! EXTRAIRE D'UN CONCEPT MODE_MECA LA DEFORMEE POUR UN OU PLUSIEURS DDL
! LES LAGRANGES SONT SUPPRIMES.
!-----------------------------------------------------------------------
! IN  :BASEMO : CONCEPT DE TYPE MODE_MECA
! IN  :NUMDDL : PERMET D'ACCEDER AU PROFIL DU CHAMP_NO EXTRAIT
! IN  :NUME   : LISTE DES NUMEROS D'ORDRE DES MODES CONSIDERES
! IN  :NBNUMO : NB DE MODES CONSIDERES
! IN  :NBEQ   : NB D'EQUATIONS
! IN  :NBNOE  : NB DE NOEUDS DU MAILLAGE
! IN  :IDDL   : LISTE DES INDICES DES DDL A EXTRAIRE
! IN  :NBDDL  : NB DE DDLS A EXTRAIRE
! OUT :DMODE  : VECTEUR => DEFORMEES MODALES
!-----------------------------------------------------------------------
!
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/rsexch.h'
    integer :: nbddl, nbnumo, nbnoe, nume(nbnumo), iddl(nbddl)
    real(kind=8) :: dmode(nbddl*nbnoe*nbnumo)
    character(len=8) :: basemo
    character(len=14) :: numddl
    character(len=24) :: deeq, nomcha
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iadmod, icm, ideeq, inumo, ipm, iret
    integer :: j, k, nbeq
!-----------------------------------------------------------------------
    call jemarq()
    deeq = numddl//'.NUME.DEEQ'
    call jeveuo(deeq, 'L', ideeq)
    ipm = 0
    icm = 0
    do 10 i = 1, nbnumo
        inumo = nume(i)
        call rsexch('F', basemo, 'DEPL', inumo, nomcha,&
                    iret)
        nomcha = nomcha(1:19)//'.VALE'
        call jeveuo(nomcha, 'L', iadmod)
        ipm = ipm + icm
        icm = 0
        do 20 j = 1, nbeq
            do 21 k = 1, nbddl
                if (zi(ideeq+(2*j)-1) .eq. iddl(k)) then
                    icm = icm + 1
                    dmode(ipm+icm) = zr(iadmod+j-1)
                    goto 22
                endif
21          continue
22          continue
20      continue
10  continue
!
    call jedema()
end subroutine
