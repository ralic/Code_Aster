subroutine tbtrtb(tabin, basout, tabout, npara, lipara,&
                  lcrit, prec, crit)
    implicit   none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/jecreo.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/tbtr01.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: npara
    real(kind=8) :: prec
    character(len=8) :: crit
    character(len=*) :: tabin, basout, tabout, lipara(*), lcrit(*)
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
!     TRI DE LA TABLE.
!     LE TRI NE PORTE QUE SUR LES TYPES I , R  ET  K
!     ARRET EN FATAL SUR LES COMPLEXES
! ----------------------------------------------------------------------
! IN  : TABIN  : NOM DE LA TABLE DONT ON VEUT TRIER DES LIGNES
! IN  : BASOUT : BASE DE CREATION DE "TABOUT"
! OUT : TABOUT : NOM DE LA TABLE QUI CONTIENDRA LES LIGNES TRIEES
! IN  : NPARA  : NOMBRE DE PARAMETRES IMPLIQUES DANS LE TRI
! IN  : LIPARA : LISTE DES PARAMETRES A TRIER
! IN  : LCRIT  : TYPES DE CRITERES: CR  CROISSANT
!                                   DE  DECROISSANT
! IN  : PREC   : PRECISION
! IN  : CRIT   : RELATIF / ABSOLU
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    integer :: iret, nbpara, nblign, jnume, jnum2, ii, jj, ktblp
    integer :: jtblp, i, j, k, n, m, ideb, ifin, nbuti, ndim
    integer :: jvall, kvall, jvale, kvale, jtbnp, ktbnp, ktbba
    character(len=1) :: base
    character(len=4) :: type, knume
    character(len=19) :: nomtab, nomta2
    character(len=24) :: nomjv, nojv2, nomjvl, nojvl2, inpar, jnpar
    character(len=24) :: valk
    logical :: lok
! ----------------------------------------------------------------------
!
    call jemarq()
!
    nomtab = tabin
    nomta2 = tabout
    base = basout(1:1)
!
!     --- VERIFICATION DE LA BASE ---
!
    ASSERT(base.eq.'V' .or. base.eq.'G')
!
!     --- VERIFICATION DE LA TABLE ---
!
    call jeexin(nomtab//'.TBBA', iret)
    if (iret .eq. 0) then
        call u2mess('F', 'UTILITAI4_64')
    endif
!
    call jeveuo(nomtab//'.TBNP', 'E', jtbnp)
    nbpara = zi(jtbnp )
    nblign = zi(jtbnp+1)
    if (nbpara .eq. 0) then
        call u2mess('F', 'UTILITAI4_65')
    endif
    if (nblign .eq. 0) then
        call u2mess('F', 'UTILITAI4_66')
    endif
!
!     --- VERIFICATION QUE LES PARAMETRES EXISTENT DANS LA TABLE ---
!
    call jeveuo(nomtab//'.TBLP', 'L', jtblp)
    do 10 i = 1, npara
        inpar = lipara(i)
        do 12 j = 1, nbpara
            jnpar = zk24(jtblp+4*(j-1))
            if (inpar .eq. jnpar) then
                type = zk24(jtblp+4*(j-1)+1)
                if (type(1:1) .eq. 'C') then
                    valk = inpar
                    call u2mesg('F', 'UTILITAI7_2', 1, valk, 0,&
                                0, 0, 0.d0)
                endif
                goto 10
            endif
12      continue
        valk = inpar
        call u2mesg('F', 'UTILITAI6_89', 1, valk, 0,&
                    0, 0, 0.d0)
10  end do
!
    call wkvect('&&TBTRTB.TRI', 'V V I', nblign, jnume)
    call wkvect('&&TBTRTB.TRI2', 'V V I', nblign, jnum2)
    do 20 i = 1, nblign
        zi(jnume+i-1) = i
20  end do
!
    call tbtr01(tabin, nbpara, lipara(1), nblign, zi(jnume))
!
    if (lcrit(1)(1:2) .eq. 'DE') then
        do 22 i = 1, nblign
            zi(jnum2-1+i) = zi(jnume-1+i)
22      continue
        do 24 i = 1, nblign
            zi(jnume-1+i) = zi(jnum2-1+nblign-i+1)
24      continue
    endif
!
    if (npara .eq. 1) then
        goto 104
    else if (npara .eq. 2) then
    else if (npara .gt. 2) then
        call u2mess('F', 'UTILITAI4_87')
    endif
    i = 1
    inpar = lipara(i)
    do 102 j = 1, nbpara
        jnpar = zk24(jtblp+4*(j-1))
        if (inpar .eq. jnpar) then
            type = zk24(jtblp+4*(j-1)+1)
            nomjv = zk24(jtblp+4*(j-1)+2)
            nomjvl = zk24(jtblp+4*(j-1)+3)
            call jeveuo(nomjv, 'L', jvale)
            call jeveuo(nomjvl, 'L', jvall)
            ii = 0
30          continue
            ii = ii + 1
            if (ii .ge. nblign) goto 32
            ideb = ii
            ifin = ii
            n = zi(jnume+ii-1)
            do 34 jj = ii+1, nblign
                ifin = jj
                m = zi(jnume+jj-1)
                if (type(1:1) .eq. 'I') then
                    if (zi(jvale+n-1) .ne. zi(jvale+m-1)) then
                        ifin = ifin - 1
                        goto 36
                    endif
                else if (type(1:1) .eq. 'R') then
!              IF ( ZR(JVALE+N-1) .NE. ZR(JVALE+M-1) ) THEN
!             TEST D'EGALITE A PREC PRES
                    if (crit .eq. 'ABSOLU  ') then
                        lok = ( abs(zr(jvale+n-1)-zr(jvale+m-1)) .le. prec*abs(zr(jvale+m-1)) )
                    else
                        lok = ( abs(zr(jvale+n-1)-zr(jvale+m-1)) .le. prec )
                    endif
                    if (.not.lok) then
                        ifin = ifin - 1
                        goto 36
                    endif
                else if (type(1:1) .eq. 'C') then
                    if (zc(jvale+n-1) .ne. zc(jvale+m-1)) then
                        ifin = ifin - 1
                        goto 36
                    endif
                else if (type(1:3) .eq. 'K80') then
                    if (zk80(jvale+n-1) .ne. zk80(jvale+m-1)) then
                        ifin = ifin - 1
                        goto 36
                    endif
                else if (type(1:3) .eq. 'K32') then
                    if (zk32(jvale+n-1) .ne. zk32(jvale+m-1)) then
                        ifin = ifin - 1
                        goto 36
                    endif
                else if (type(1:3) .eq. 'K24') then
                    if (zk24(jvale+n-1) .ne. zk24(jvale+m-1)) then
                        ifin = ifin - 1
                        goto 36
                    endif
                else if (type(1:3) .eq. 'K16') then
                    if (zk16(jvale+n-1) .ne. zk16(jvale+m-1)) then
                        ifin = ifin - 1
                        goto 36
                    endif
                else if (type(1:2) .eq. 'K8') then
                    if (zk8(jvale+n-1) .ne. zk8(jvale+m-1)) then
                        ifin = ifin - 1
                        goto 36
                    endif
                endif
34          continue
36          continue
            nbuti = ifin-ideb+1
            if (nbuti .gt. 1) then
!           --- ON TESTE AVEC LE PARAMETRE SUIVANT ---
                call tbtr01(tabin, nbpara, lipara(i+1), nbuti, zi(jnume-1+ideb))
                if (lcrit(i+1)(1:2) .eq. 'DE') then
                    do 40 k = 1, nbuti
                        zi(jnum2+k-1) = zi(jnume-1+ideb+k-1)
40                  continue
                    do 42 k = 1, nbuti
                        zi(jnume-1+ideb+k-1) = zi(jnum2-1+nbuti-k+1)
42                  continue
                endif
            endif
            ii = ifin
            goto 30
32          continue
            goto 104
        endif
102  end do
104  continue
!
!     --- ON DUPLIQUE LA TABLE ---
!
!     -- .TBBA :
    call wkvect(nomta2//'.TBBA', base//' V K8', 1, ktbba)
    zk8(ktbba) = base
!
!     -- .TBNP :
    call wkvect(nomta2//'.TBNP', base//' V I', 2, ktbnp)
    zi(ktbnp ) = nbpara
    zi(ktbnp+1) = nblign
!
!     -- .TBLP :
    ndim = 4 * nbpara
    call jecreo(nomta2//'.TBLP', base//' V K24')
    call jeecra(nomta2//'.TBLP', 'LONMAX', ndim, ' ')
    call jeecra(nomta2//'.TBLP', 'LONUTI', ndim, ' ')
    call jeveuo(nomta2//'.TBLP', 'E', ktblp)
    do 300 i = 1, nbpara
        zk24(ktblp+4*(i-1) ) = zk24(jtblp+4*(i-1) )
        zk24(ktblp+4*(i-1)+1) = zk24(jtblp+4*(i-1)+1)
!
        call codent(i, 'D0', knume)
        nomjv = nomta2//'.'//knume
        zk24(ktblp+4*(i-1)+2) = nomjv
        type = zk24(jtblp+4*(i-1)+1)
        call jecreo(nomjv, base//' V '//type)
        call jeecra(nomjv, 'LONMAX', nblign, ' ')
        call jeecra(nomjv, 'LONUTI', nblign, ' ')
        call jeveuo(nomjv, 'E', kvale)
!
        nomjv = nomta2(1:17)//'LG.'//knume
        zk24(ktblp+4*(i-1)+3) = nomjv
        call jecreo(nomjv, base//' V I')
        call jeecra(nomjv, 'LONMAX', nblign, ' ')
        call jeveuo(nomjv, 'E', kvall)
!
        nojv2 = zk24(jtblp+4*(i-1)+2)
        nojvl2 = zk24(jtblp+4*(i-1)+3)
        call jeveuo(nojv2, 'L', jvale)
        call jeveuo(nojvl2, 'L', jvall)
!
        do 302 j = 1, nblign
            zi(kvall+j-1) = zi(jvall+zi(jnume+j-1)-1)
            if (type(1:1) .eq. 'I') then
                zi(kvale+j-1) = zi(jvale+zi(jnume+j-1)-1)
            else if (type(1:1) .eq. 'R') then
                zr(kvale+j-1) = zr(jvale+zi(jnume+j-1)-1)
            else if (type(1:1) .eq. 'C') then
                zc(kvale+j-1) = zc(jvale+zi(jnume+j-1)-1)
            else if (type(1:3) .eq. 'K80') then
                zk80(kvale+j-1) = zk80(jvale+zi(jnume+j-1)-1)
            else if (type(1:3) .eq. 'K32') then
                zk32(kvale+j-1) = zk32(jvale+zi(jnume+j-1)-1)
            else if (type(1:3) .eq. 'K24') then
                zk24(kvale+j-1) = zk24(jvale+zi(jnume+j-1)-1)
            else if (type(1:3) .eq. 'K16') then
                zk16(kvale+j-1) = zk16(jvale+zi(jnume+j-1)-1)
            else if (type(1:3) .eq. 'K8') then
                zk8(kvale+j-1) = zk8(jvale+zi(jnume+j-1)-1)
            endif
302      continue
!
300  end do
!
    call jedetr('&&TBTRTB.TRI')
    call jedetr('&&TBTRTB.TRI2')
!
    call jedema()
end subroutine
