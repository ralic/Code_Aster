subroutine caelec(char, ligrmo, noma)
    implicit   none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/alcart.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nocart.h"
#include "asterfort/reliem.h"
    character(len=8) :: char, noma
    character(len=*) :: ligrmo
! ----------------------------------------------------------------------
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
! BUT : STOCKAGE DES CHARGES REPARTIES DANS UNE CARTE ALLOUEE SUR LE
!       LIGREL DU MODELE
!
! ARGUMENTS D'ENTREE:
!      CHAR   : NOM UTILISATEUR DU RESULTAT DE CHARGE
!      LIGRMO : NOM DU LIGREL DE MODELE
!      NOMA   : NOM DU MAILLAGE
! ----------------------------------------------------------------------
    integer :: i, n, nbfel, jvalv, jncmp, iocc, nbtou, nbma, jma
    real(kind=8) :: p1(3), p2(3), zcod, d
    character(len=8) :: k8b, code, typmcl(2)
    character(len=16) :: motclf, motcle(2)
    character(len=19) :: carte
    character(len=24) :: mesmai
    integer :: iarg
!     ------------------------------------------------------------------
!
    call jemarq()
!
    motclf = 'FORCE_ELEC'
    call getfac(motclf, nbfel)
!
    carte = char//'.CHME.'//'FELEC'
    call alcart('G', carte, noma, 'FELECR')
!
    call jeveuo(carte//'.NCMP', 'E', jncmp)
    call jeveuo(carte//'.VALV', 'E', jvalv)
!
! --- STOCKAGE DE FORCES NULLES SUR TOUT LE MAILLAGE
!
    zk8(jncmp-1+1) = 'X1'
    zk8(jncmp-1+2) = 'Y1'
    zk8(jncmp-1+3) = 'Z1'
    zk8(jncmp-1+4) = 'X2'
    zk8(jncmp-1+5) = 'Y2'
    zk8(jncmp-1+6) = 'Z2'
    zk8(jncmp-1+7) = 'CODE'
    do 100 i = 1, 7
        zr(jvalv-1+i) = 0.d0
100  end do
    call nocart(carte, 1, ' ', 'NOM', 0,&
                ' ', 0, ligrmo, 7)
!
    mesmai = '&&CAELEC.MES_MAILLES'
    motcle(1) = 'GROUP_MA'
    motcle(2) = 'MAILLE'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
!
! --- STOCKAGE DANS LA CARTE
!
    do 120 iocc = 1, nbfel
!
        call getvtx(motclf, 'POSITION', iocc, iarg, 1,&
                    code, n)
!
        if (n .eq. 0) then
            zcod = 10.d0
            call getvr8(motclf, 'FX', iocc, iarg, 1,&
                        p1(1), n)
            call getvr8(motclf, 'FY', iocc, iarg, 1,&
                        p1(2), n)
            call getvr8(motclf, 'FZ', iocc, iarg, 1,&
                        p1(3), n)
            p2(1) = 0.d0
            p2(2) = 0.d0
            p2(3) = 0.d0
        else
            if (code .eq. 'PARA') then
                call getvr8(motclf, 'DIST', iocc, iarg, 1,&
                            d, n)
                if (n .ne. 0) then
                    zcod = 12.d0
                    p1(1)=d
                    p1(2)=0.d0
                    p1(3)=0.d0
                    call getvr8(motclf, 'POINT2', iocc, iarg, 3,&
                                p2, n)
                else
                    zcod = 11.d0
                    call getvr8(motclf, 'TRANS', iocc, iarg, 3,&
                                p1, n)
                    p2(1)=0.d0
                    p2(2)=0.d0
                    p2(3)=0.d0
                endif
            else if (code.eq.'INFI') then
                zcod = 2.d0
                call getvr8(motclf, 'POINT1', iocc, iarg, 3,&
                            p1, n)
                call getvr8(motclf, 'POINT2', iocc, iarg, 3,&
                            p2, n)
            else if (code.eq.'FINI') then
                zcod = 3.d0
                call getvr8(motclf, 'POINT1', iocc, iarg, 3,&
                            p1, n)
                call getvr8(motclf, 'POINT2', iocc, iarg, 3,&
                            p2, n)
            endif
        endif
!
        zr(jvalv-1+1) = p1(1)
        zr(jvalv-1+2) = p1(2)
        zr(jvalv-1+3) = p1(3)
        zr(jvalv-1+4) = p2(1)
        zr(jvalv-1+5) = p2(2)
        zr(jvalv-1+6) = p2(3)
        zr(jvalv-1+7) = zcod
!
        call getvtx(motclf, 'TOUT', iocc, iarg, 1,&
                    k8b, nbtou)
!
        if (nbtou .ne. 0) then
!
            call nocart(carte, 1, ' ', 'NOM', 0,&
                        ' ', 0, ligrmo, 7)
        else
            call reliem(ligrmo, noma, 'NU_MAILLE', motclf, iocc,&
                        2, motcle, typmcl, mesmai, nbma)
            if (nbma .eq. 0) goto 120
            call jeveuo(mesmai, 'L', jma)
            call nocart(carte, 3, k8b, 'NUM', nbma,&
                        k8b, zi(jma), ' ', 7)
            call jedetr(mesmai)
        endif
!
120  end do
!
    call jedema()
end subroutine
