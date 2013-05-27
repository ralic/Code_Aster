function typmat(nbmat, tlimat)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
    include 'asterfort/assert.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/mpicm1.h'
    include 'asterfort/redetr.h'
    character(len=*) :: tlimat(*)
    integer :: nbmat
    character(len=1) :: typmat
!-----------------------------------------------------------------------
!
!  BUT : CETTE FONCTION RETOURNE LE TYPE SYMETRIQUE : 'S'
!                                     OU PLEINE     : 'N'
!        DE LA MATRICE GLOBALE RESULTANTE DE L'ASSEMBLAGE
!        DES MATR_ELEM TLIMAT
!  ATTENTION :
!    SI ON EST EN PARALLELE MPI, LA REPONSE EST GLOBALE.
!    CETTE ROUTINE FAIT DU MPI_ALLREDUCE. IL FAUT DONC L'APPELER
!    DE LA MEME FACON SUR TOUS LES PROCS.
!
!-----------------------------------------------------------------------
! --- DESCRIPTION DES PARAMETRES
! IN  I  NBMAT  : NOMBRE DE MATR_ELEM DE LA LISTE TLIMAT
! IN  K* TLIMAT : LISTE DES MATR_ELEM
! ----------------------------------------------------------------------
    real(kind=8) :: rbid
    complex(kind=8) :: cbid
!----------------------------------------------------------------------
    character(len=8) :: sym, zero
    character(len=19) :: matel
    integer :: i, itymat
    integer :: ibid, ierd
    integer :: iexi, iexiav
!----------------------------------------------------------------------
!     ITYMAT =  0 -> SYMETRIQUE
!            =  1 -> NON-SYMETRIQUE
!
!     --  PAR DEFAUT LE TYPE DE MATRICE EST SYMETRIQUE
    itymat = 0
!
    do 10 i = 1, nbmat
        matel = tlimat(i)
        call jeexin(matel//'.RELR', iexi)
        iexi=min(1,abs(iexi))
        iexiav=iexi
        call mpicm1('MPI_MAX', 'I', 1, ibid, iexi,&
                    rbid, cbid)
        iexi=min(1,abs(iexi))
        call assert(iexi.eq.iexiav)
        if (iexi .eq. 0) goto 10
!
!       -- LA LOGIQUE CI-DESSOUS N'EST VALABLE QUE SI LE MATR_ELEM
!          A ETE EXPURGE DE SES RESUELEM NULS => CALL REDETR()
        call redetr(matel)
!
        call dismoi('F', 'TYPE_MATRICE', matel, 'MATR_ELEM', ibid,&
                    sym, ierd)
        if (sym .eq. 'NON_SYM') then
            call dismoi('F', 'ZERO', matel, 'MATR_ELEM', ibid,&
                        zero, ierd)
            if (zero .eq. 'NON') then
                itymat = 1
            endif
        endif
!
        call mpicm1('MPI_MAX', 'I', 1, ibid, itymat,&
                    rbid, cbid)
        if (itymat .eq. 1) goto 11
!
10  end do
11  continue
!
    if (itymat .eq. 0) then
        typmat='S'
    else
        typmat='N'
    endif
end function
