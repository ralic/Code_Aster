subroutine op0031()
    implicit none
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
!
!     COMBINAISON LINEAIRE DE MATRICE
!     ------------------------------------------------------------------
!
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterfort/amogen.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/getvc8.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mtcmbl.h"
#include "asterfort/mtdefs.h"
#include "asterfort/u2mesk.h"
#include "asterfort/vrrefe.h"
#include "asterfort/wkvect.h"
    character(len=1) :: typres, base
    character(len=6) :: combrc
    character(len=8) :: matres, matri1, partie
    character(len=19) :: matr19, nomi
    character(len=8) :: nomddl, mode
    character(len=14) :: numgen
    character(len=16) :: concep, nomcmd, typrep, rep2
    character(len=24) :: cnom, ccoef, ctypec, valk(2)
    real(kind=8) :: r8val(2)
    logical :: lcoefc, lreent
    complex(kind=8) :: cval
    integer :: nbocag, nboccr, nboccc, nbocc, ldesc, l, iref1, iref2, i, lnom
    integer :: iocc
    integer :: ibid, lcoef, ltypec, nbcst, lr, lc, iret, ides1
    integer :: jrefe, jpomr
    integer :: iarg
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
    call getres(matres, concep, nomcmd)
    call gettco(matres, typrep)
    matr19=matres
!
!     -- LA MATRICE RESULTAT  EST-ELLE REENTRANTE ?
    call jeexin(matr19//'.REFA', iret)
    lreent=(iret.ne.0)
    base='G'
    if (lreent) then
        matr19='&&OP0031.MATRES'
        base='V'
    endif
!
    call getfac('CALC_AMOR_GENE', nbocag)
    if (nbocag .ne. 0) then
        call amogen(matr19)
        goto 9999
    endif
!
    call getfac('COMB_R', nboccr)
    call getfac('COMB_C', nboccc)
    if (nboccr .ne. 0) then
        nbocc = nboccr
        typres = 'R'
        combrc = 'COMB_R'
    else
        nbocc = nboccc
        typres = 'C'
        combrc = 'COMB_C'
    endif
!
!
!
    if (typrep(1:14) .eq. 'MATR_ASSE_GENE') then
!         CREATION D UN .DESC POUR LA MATRASS GENE RESULTAT
        call wkvect(matr19//'.DESC', base//' V I', 3, ldesc)
        if (nbocc .ne. 0) then
            call getvid(combrc, 'MATR_ASSE', iocc=1, scal=matri1, nbret=l)
            call jeveuo(matri1//'           .REFA', 'L', iref1)
            numgen = zk24(iref1+1)(1:14)
            call jeveuo(numgen//'.NUME.REFN', 'L', iref2)
            mode = zk24(iref2)(1:8)
            call gettco(mode, rep2)
            if (rep2(1:11) .ne. 'MODELE_GENE') then
                call jeveuo(matri1//'           .DESC', 'L', ides1)
                do 1 i = 1, 3
                    zi(ldesc+i-1)=zi(ides1+i-1)
 1              continue
            endif
        endif
    endif
!
!
    cnom = '&&OP0031.LISTE_MATRICE'
    call wkvect(cnom, 'V V K8', nbocc, lnom)
    jpomr=0
    do 10 iocc = 0, nbocc - 1
        call getvid(combrc, 'MATR_ASSE', iocc=iocc+1, scal=zk8(lnom+iocc), nbret=l)
!        ON RECHERCHE UNE EVENTUELLE MATRICE NON SYMETRIQUE
        nomi=zk8(lnom+iocc)
        call jeveuo(nomi//'.REFA', 'L', jrefe)
        if (zk24(jrefe-1+9) .eq. 'MR') then
            jpomr=iocc
        endif
10  end do
!
!
    nomddl=' '
    call getvtx(' ', 'SANS_CMP', scal=nomddl, nbret=ibid)
!
!
!     --- RECUPERATION DES COEFFICIENTS :
!     ------------------------------------
!       REMARQUE : POUR PARTIE='IMAG', ON FAIT COEF=-J*COEF
!
    ccoef = '&&OP0031.COEF_VALEURS'
    call wkvect(ccoef, 'V V R', 2*nbocc, lcoef)
    ctypec = '&&OP0031.COEF_TYPE'
    call wkvect(ctypec, 'V V K8', nbocc, ltypec)
!
    nbcst = 0
    do 25 iocc = 0, nbocc - 1
        call getvr8(combrc, 'COEF_R', iocc=iocc+1, scal=r8val(1), nbret=lr)
        if (lr .eq. 1) then
            lcoefc=.false.
            if (combrc .eq. 'COMB_R') then
                partie=' '
                call getvtx(combrc, 'PARTIE', iocc=iocc+1, scal=partie, nbret=ibid)
                if (partie .eq. 'IMAG') lcoefc=.true.
            endif
!
            if (.not.lcoefc) then
                zr(lcoef+nbcst) = r8val(1)
                nbcst = nbcst + 1
                zk8(ltypec+iocc) = 'R'
            else
                zr(lcoef+nbcst) = 0.d0
                zr(lcoef+nbcst+1) = -1.d0*r8val(1)
                nbcst = nbcst + 2
                zk8(ltypec+iocc) = 'C'
            endif
        else
            call getvc8(combrc, 'COEF_C', iocc=iocc+1, scal=cval, nbret=lc)
            ASSERT(lc.eq.1)
            zr(lcoef+nbcst) = dble(cval)
            zr(lcoef+nbcst+1) = dimag(cval)
            nbcst = nbcst + 2
            zk8(ltypec+iocc) = 'C'
        endif
25  end do
!
!
!     --- CONTROLE DES REFERENCES :
!     --------------------------------
    do 30 iocc = 0, nbocc - 2
        call vrrefe(zk8(lnom+iocc), zk8(lnom+iocc+1), iret)
        if (iret .ne. 0) then
            valk(1)=zk8(lnom+iocc)
            valk(2)=zk8(lnom+iocc+1)
            call u2mesk('F', 'ALGELINE2_28', 2, valk)
        endif
30  end do
!
!
!
!     -- COMBINAISON DES MATRICES :
!     ------------------------------------------------------------------
    call mtdefs(matr19, zk8(lnom+jpomr), base, typres)
    call mtcmbl(nbocc, zk8(ltypec), zr(lcoef), zk8(lnom), matr19,&
                nomddl, ' ', 'ELIM=')
!
!
!
!     -- SI LA MATRICE EST REENTRANTE, ON LA DETRUIT ET ON RECOPIE
!        LA MATRICE INTERMEDIAIRE :
    if (lreent) then
        ASSERT(matr19(1:8).eq.'&&OP0031')
        call detrsd('MATR_ASSE', matres)
        call copisd('MATR_ASSE', 'G', matr19, matres)
        call detrsd('MATR_ASSE', matr19)
    endif
!
9999  continue
    call jedema()
end subroutine
