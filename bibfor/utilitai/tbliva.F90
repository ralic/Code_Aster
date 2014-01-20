subroutine tbliva(nomta, npacri, lipacr, vi, vr,&
                  vc, vk, crit, prec, para,&
                  ctype, vali, valr, valc, valk,&
                  ier)
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    integer :: npacri, vi(*), vali, ier
    real(kind=8) :: vr(*), valr, prec(*)
    complex(kind=8) :: vc(*), valc
    character(len=*) :: nomta, lipacr(*), vk(*), valk, crit(*), ctype, para
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
!      LECTURE D'UNE VALEUR D'UNE CELLULE DE LA TABLE.
! ----------------------------------------------------------------------
! IN  : NOMTA  : NOM DE LA STRUCTURE "TABLE".
! IN  : NPACRI : NOMBRE DE PARAMETRES IMPLIQUES DANS LES CRITERES
! IN  : LIPACR : LISTE DES PARAMETRES CRITERES
! IN  : VI     : LISTE DES CRITERES POUR LES PARAMETRES "I"
! IN  : VR     : LISTE DES CRITERES POUR LES PARAMETRES "R"
! IN  : VC     : LISTE DES CRITERES POUR LES PARAMETRES "C"
! IN  : VK     : LISTE DES CRITERES POUR LES PARAMETRES "K"
! IN  : CRIT   : CRITERE POUR LES PARAMETRES REELS
! IN  : PREC   : PRECISION POUR LES PARAMETRES REELS
! IN  : PARA   : PARAMETRE A TROUVER
! OUT : CTYPE  : TYPE DE LA VALEUR TROUVEE
! OUT : VALI   : VALEUR TROUVEE SI PARAMETRES "I"
! OUT : VALR   : VALEUR TROUVEE SI PARAMETRES "R"
! OUT : VALC   : VALEUR TROUVEE SI PARAMETRES "C"
! OUT : VALK   : VALEUR TROUVEE SI PARAMETRES "K"
! OUT : IER    : CODE RETOUR 0 : OK
!                            1 : PARA N'EXISTE PAS
!                            2 : PAS DE LIGNE TROUVEE
!                            3 : PLUSIEURS LIGNES TROUVEES
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    integer :: iret, nbpara, nblign, jtbnp, nbpu
    integer :: jtblp, i, j, k, n, jvale, itrouv
    integer :: ki, kr, kc, k8, jvall
    real(kind=8) :: refr, xr, epsi
    complex(kind=8) :: refc, xc
    character(len=4) :: rela, type
    character(len=19) :: nomtab
    character(len=24) :: nomjv, nomjvl, inpar, jnpar
    logical :: lok
    integer, pointer :: numero(:) => null()
! ----------------------------------------------------------------------
!
    call jemarq()
!
    ier = 0
    ctype = '?'
!
    vali=0
    valr=0.d0
    valc=dcmplx(0.d0,0.d0)
    valk=' '
!
    nomtab = nomta
    call jeexin(nomtab//'.TBBA', iret)
    if (iret .eq. 0) then
        call utmess('F', 'UTILITAI4_64')
    endif
!
    call jeveuo(nomtab//'.TBNP', 'E', jtbnp)
    nbpara = zi(jtbnp )
    nblign = zi(jtbnp+1)
    if (nbpara .eq. 0) then
        call utmess('F', 'UTILITAI4_65')
    endif
    if (nblign .eq. 0) then
        call utmess('F', 'UTILITAI4_66')
    endif
!
    call jeveuo(nomtab//'.TBLP', 'L', jtblp)
!
!     --- VERIFICATION QUE LES PARAMETRES EXISTENT DANS LA TABLE ---
!
    do 10 i = 1, npacri
        inpar = lipacr(i)
        do 12 j = 1, nbpara
            jnpar = zk24(jtblp+4*(j-1))
            if (inpar .eq. jnpar) goto 10
12      continue
        ier = 1
        goto 9999
10  end do
    inpar = para
    do 14 j = 1, nbpara
        jnpar = zk24(jtblp+4*(j-1))
        if (inpar .eq. jnpar) goto 16
14  end do
    ier = 1
    goto 9999
16  continue
!
    nomjv = zk24(jtblp+2)
    call jelira(nomjv, 'LONUTI', nbpu)
    AS_ALLOCATE(vi=numero, size=nbpu)
    do 18 i = 1, nbpu
        numero(i) = i
18  end do
!
    ki = 0
    kr = 0
    kc = 0
    k8 = 0
    do 20 i = 1, npacri
        itrouv = 0
        inpar = lipacr(i)
        do 22 j = 1, nbpara
            jnpar = zk24(jtblp+4*(j-1))
            if (inpar .eq. jnpar) then
                type = zk24(jtblp+4*(j-1)+1)
                nomjv = zk24(jtblp+4*(j-1)+2)
                nomjvl = zk24(jtblp+4*(j-1)+3)
                call jeveuo(nomjv, 'L', jvale)
                call jeveuo(nomjvl, 'L', jvall)
                if (type(1:1) .eq. 'I') then
                    ki = ki + 1
                    do 30 k = 1, nbpu
                        n = numero(k)
                        if (zi(jvall+n-1) .eq. 0) goto 30
                        if (zi(jvale+n-1) .eq. vi(ki)) then
                            itrouv = itrouv + 1
                            numero(itrouv) = n
                        endif
30                  continue
                else if (type(1:1) .eq. 'R') then
                    kr = kr + 1
                    rela = crit(kr)
                    epsi = prec(kr)
                    xr = vr(kr)
                    do 31 k = 1, nbpu
                        n = numero(k)
                        if (zi(jvall+n-1) .eq. 0) goto 31
                        refr = zr(jvale+n-1)
                        if (rela .eq. 'RELA') then
                            lok = ( abs(xr-refr) .le. epsi * abs(refr) )
                        else if (rela .eq. 'EGAL') then
                            lok = ( refr .eq. xr )
                        else
                            lok = ( abs(xr - refr) .le. epsi )
                        endif
                        if (lok) then
                            itrouv = itrouv + 1
                            numero(itrouv) = n
                        endif
31                  continue
                else if (type(1:1) .eq. 'C') then
                    kc = kc + 1
                    rela = crit(kc)
                    epsi = prec(kc)
                    xc = vc(kc)
                    do 32 k = 1, nbpu
                        n = numero(k)
                        if (zi(jvall+n-1) .eq. 0) goto 32
                        refc = zc(jvale+n-1)
                        if (rela .eq. 'RELA') then
                            lok = ( abs(xc-refc) .le. epsi * abs(refc) )
                        else
                            lok = ( abs(xc - refc) .le. epsi )
                        endif
                        if (lok) then
                            itrouv = itrouv + 1
                            numero(itrouv) = n
                        endif
32                  continue
                else if (type(1:3) .eq. 'K80') then
                    k8 = k8 + 1
                    do 33 k = 1, nbpu
                        n = numero(k)
                        if (zi(jvall+n-1) .eq. 0) goto 33
                        if (zk80(jvale+n-1) .eq. vk(k8)) then
                            itrouv = itrouv + 1
                            numero(itrouv) = n
                        endif
33                  continue
                else if (type(1:3) .eq. 'K32') then
                    k8 = k8 + 1
                    do 34 k = 1, nbpu
                        n = numero(k)
                        if (zi(jvall+n-1) .eq. 0) goto 34
                        if (zk32(jvale+n-1) .eq. vk(k8)) then
                            itrouv = itrouv + 1
                            numero(itrouv) = n
                        endif
34                  continue
                else if (type(1:3) .eq. 'K24') then
                    k8 = k8 + 1
                    do 35 k = 1, nbpu
                        n = numero(k)
                        if (zi(jvall+n-1) .eq. 0) goto 35
                        if (zk24(jvale+n-1) .eq. vk(k8)) then
                            itrouv = itrouv + 1
                            numero(itrouv) = n
                        endif
35                  continue
                else if (type(1:3) .eq. 'K16') then
                    k8 = k8 + 1
                    do 36 k = 1, nbpu
                        n = numero(k)
                        if (zi(jvall+n-1) .eq. 0) goto 36
                        if (zk16(jvale+n-1) .eq. vk(k8)) then
                            itrouv = itrouv + 1
                            numero(itrouv) = n
                        endif
36                  continue
                else if (type(1:2) .eq. 'K8') then
                    k8 = k8 + 1
                    do 37 k = 1, nbpu
                        n = numero(k)
                        if (zi(jvall+n-1) .eq. 0) goto 37
                        if (zk8(jvale+n-1) .eq. vk(k8)) then
                            itrouv = itrouv + 1
                            numero(itrouv) = n
                        endif
37                  continue
                endif
            endif
22      continue
        if (itrouv .eq. 0) then
            ier = 2
            goto 9999
        endif
        nbpu = itrouv
20  end do
!
    itrouv = 0
    inpar = para
    do 40 j = 1, nbpara
        jnpar = zk24(jtblp+4*(j-1))
        if (inpar .eq. jnpar) then
            type = zk24(jtblp+4*(j-1)+1)
            nomjv = zk24(jtblp+4*(j-1)+2)
            nomjvl = zk24(jtblp+4*(j-1)+3)
            call jeveuo(nomjv, 'L', jvale)
            call jeveuo(nomjvl, 'L', jvall)
            if (type(1:1) .eq. 'I') then
                do 50 k = 1, nbpu
                    n = numero(k)
                    if (zi(jvall+n-1) .eq. 0) goto 50
                    itrouv = itrouv + 1
                    vali = zi(jvale+n-1)
                    ctype = 'I'
50              continue
            else if (type(1:1) .eq. 'R') then
                do 51 k = 1, nbpu
                    n = numero(k)
                    if (zi(jvall+n-1) .eq. 0) goto 51
                    itrouv = itrouv + 1
                    valr = zr(jvale+n-1)
                    ctype = 'R'
51              continue
            else if (type(1:1) .eq. 'C') then
                do 52 k = 1, nbpu
                    n = numero(k)
                    if (zi(jvall+n-1) .eq. 0) goto 52
                    itrouv = itrouv + 1
                    valc = zc(jvale+n-1)
                    ctype = 'C'
52              continue
            else if (type(1:3) .eq. 'K80') then
                k8 = k8 + 1
                do 53 k = 1, nbpu
                    n = numero(k)
                    if (zi(jvall+n-1) .eq. 0) goto 53
                    itrouv = itrouv + 1
                    valk = zk80(jvale+n-1)
                    ctype = 'K'
53              continue
            else if (type(1:3) .eq. 'K32') then
                do 54 k = 1, nbpu
                    n = numero(k)
                    if (zi(jvall+n-1) .eq. 0) goto 54
                    itrouv = itrouv + 1
                    valk = zk32(jvale+n-1)
                    ctype = 'K'
54              continue
            else if (type(1:3) .eq. 'K24') then
                do 55 k = 1, nbpu
                    n = numero(k)
                    if (zi(jvall+n-1) .eq. 0) goto 55
                    itrouv = itrouv + 1
                    valk = zk24(jvale+n-1)
                    ctype = 'K'
55              continue
            else if (type(1:3) .eq. 'K16') then
                do 56 k = 1, nbpu
                    n = numero(k)
                    if (zi(jvall+n-1) .eq. 0) goto 56
                    itrouv = itrouv + 1
                    valk = zk16(jvale+n-1)
                    ctype = 'K'
56              continue
            else if (type(1:2) .eq. 'K8') then
                do 57 k = 1, nbpu
                    n = numero(k)
                    if (zi(jvall+n-1) .eq. 0) goto 57
                    itrouv = itrouv + 1
                    valk = zk8(jvale+n-1)
                    ctype = 'K'
57              continue
            endif
        endif
40  end do
!
    if (itrouv .eq. 0) ier = 2
    if (itrouv .gt. 1) ier = 3
!
9999  continue
    AS_DEALLOCATE(vi=numero)
!
    call jedema()
end subroutine
