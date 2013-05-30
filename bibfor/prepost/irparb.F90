subroutine irparb(resu, nbin, parin, nomjv, nbout)
    implicit none
    include 'jeveux.h'
    include 'asterc/getres.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/rsexpa.h'
    include 'asterfort/rsnopa.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: resu, parin(*), nomjv
    integer :: nbin, nbout
!     ------------------------------------------------------------------
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
!     DETERMINATION / VERIFICATION DES PARAMETRES
!     ------------------------------------------------------------------
! IN  RESU   : K8  : NOM DU CONCEPT
! IN  NBIN   : I   : NOMBRE DE PARAMETRES EN ENTREE
! IN  PARIN  : K16 : LISTE DES PARAMETRES EN ENTREE
! IN  NOMJV  : K16 : NOM DE L'OBJET JEVEUX DE STOCKAGE DES
!                       NOMS DE PARAMETRES
! OUT NBOUT  : I   : NOMBRE DE PARAMETRES EN SORTIE
!     ------------------------------------------------------------------
    character(len=24) :: valk(2)
!     ------------------------------------------------------------------
    integer :: nbac, nbpa
    character(len=8) :: resu8
    character(len=16) :: cbid, nomcmd
!
!-----------------------------------------------------------------------
    integer :: i, iret, lpout
!-----------------------------------------------------------------------
    call jemarq()
    resu8 = resu
!
    if (nbin .eq. 0) then
        nbout = 0
        lpout = 1
    else if (nbin .lt. 0) then
        call rsnopa(resu8, 2, nomjv, nbac, nbpa)
        call jeexin(nomjv, iret)
        if (iret .gt. 0) call jeveuo(nomjv, 'E', lpout)
        nbout = nbac + nbpa
    else
!
!       --- VERIFICATION DE L'EXISTANCE DU PARAMETRE
        nbout = 0
        call jeexin(nomjv, iret)
        if (iret .ne. 0) call jedetr(nomjv)
        call wkvect(nomjv, 'V V K16', nbin, lpout)
        do 225 i = 1, nbin
            call rsexpa(resu8, 2, parin(i), iret)
            if (iret .eq. 0) then
                call getres(cbid, cbid, nomcmd)
                valk (1) = parin(i)
                valk (2) = ' '
                call u2mesg('A', 'PREPOST5_41', 2, valk, 0,&
                            0, 0, 0.d0)
            else
                nbout = nbout + 1
                zk16(lpout+nbout-1) = parin(i)
            endif
225      continue
    endif
!
!
    call jedema()
end subroutine
