subroutine gcharm(fonc, charg, nomfon, nomf, time,&
                  iord, chargm)
    implicit none
    include 'jeveux.h'
    include 'asterfort/codent.h'
    include 'asterfort/copisd.h'
    include 'asterfort/fointe.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedupo.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/u2mesk.h'
    character(len=19) :: charg, chargm
    character(len=24) :: nomfon
    character(len=8) :: nomf
    integer :: iord
    real(kind=8) :: time
    logical :: fonc
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
!
!     BUT : APPLIQUE LA FONCTION MULTIPLICATRICE SUR UNE CHARGE
!           (ROUTINE SPECIFIQUE A L'OPERATEUR CALC_G,
!            APPELEE PAR GCHARG)
!
!     IN :    FONC    :  = .TRUE. SI LE CHARGEMENT EST 'FONCTION'
!                        = .FALSE. SI LE CHARGEMENT EST 'SCALAIRE'
!             CHARG   : NOM DE LA CHARGE (ex: NOMCHA//'.CHME.F3D3D')
!             NOMFON  : NOM DE LA FONCTION MULTIPLICATRICE
!             TIME    : INSTANT (ABSCISSE DE NOMFON)
!             IORD    : NUMERO D'ORDRE CORRESPONDANT A TIME
!     IN/OUT  NOMF    : UTILISE UNIQUEMENT POUR UN CHARGEMENT 'FONCTION'
!                       PERMET DE DEFINIR LE NOM DES NOUVELLES FONCTIONS
!                       DE CHARGM (.VALE)
!             CHARGM  : CHARGE APRES LA PRISE EN COMPTE
!                       DE LA FONCTION MULTIPLICATRICE
! ======================================================================
! ----------------------------------------------------------------------
!     ------------------------------------------------------------------
!
    integer :: ival, jval, nbvale, iret, in, k, i, nb, npt, jvalf, iprol
    real(kind=8) :: const
    character(len=8) :: k8b
    character(len=19) :: nch19
!
    call jemarq()
!
    call fointe('A', nomfon, 1, 'INST', time,&
                const, iret)
!
    call jeveuo(charg//'.VALE', 'L', ival)
    call jeveuo(chargm//'.VALE', 'E', jval)
    call jelira(charg//'.VALE', 'LONMAX', nbvale, k8b)
!
! --- 1. CHARGEMENT 'SCALAIRE'
!
    if (.not.fonc) then
        do 10 in = 1, nbvale
            zr(jval+in-1) = const* zr(ival +in-1)
10      continue
!
! --- 2. CHARGEMENT 'FONCTION'
!
    else
        k=0
        call codent(iord, 'D0', nomf(4:5))
        do 20 in = 1, nbvale
            if (zk8(ival+in-1)(1:7) .ne. '&FOZERO' .and. zk8(ival+in-1)(1:7) .ne. '       '&
                .and. zk8(ival+in-1)(1:6) .ne. 'GLOBAL') then
                k=k+1
                call codent(k, 'D0', nomf(8:8))
                call copisd('FONCTION', 'V', zk8(ival+in-1), nomf)
                nch19=nomf
                call jeveuo(nch19//'.PROL', 'L', iprol)
                if (zk24(iprol)(1:8) .ne. 'INTERPRE') then
                    call jeveuo(nch19//'.VALE', 'E', jvalf)
                    call jelira(nch19//'.VALE', 'LONMAX', nb, k8b)
                    npt=nb/2
                    do 30 i = 1, npt
                        zr(jvalf+npt+i-1)=const*zr(jvalf+npt+i-1)
30                  continue
                    zk8(jval+in-1)=nomf
                else
                    call u2mesk('A', 'CALCULEL5_56', 1, charg(1:8))
                    call jedupo(charg//'.VALE', 'V', chargm//'.VALE', .false.)
                    goto 9999
                endif
            endif
20      continue
    endif
!
9999  continue
    call jedema()
!
end subroutine
