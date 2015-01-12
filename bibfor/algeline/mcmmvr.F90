subroutine mcmmvr(cumul, lmat, smdi, smhc, neq,&
                  vect, xsol, nbvect, vectmp, prepos)
! aslint: disable=W1304
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mcconl.h"
    character(len=*) :: cumul
    integer(kind=4) :: smhc(*)
    integer :: smdi(*), neq, nbvect, lmat
    complex(kind=8) :: vect(neq, nbvect), xsol(neq, nbvect), vectmp(neq)
    aster_logical :: prepos
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
!     ------------------------------------------------------------------
!                   MULTIPLICATION MATRICE PAR N VECTEURS
!         XSOL(1..NEQ,1..NBVECT) = MATRICE  * VECT(1..NEQ,1..NBVECT)
!     ------------------------------------------------------------------
!     VERSION : LA MATRICE EST REELLE SYMETRIQUE OU NON (MORSE)
!             : LES VECTEURS SONT COMPLEXES
!     ------------------------------------------------------------------
!
!
!
    integer :: jmat1, jmat2, nbloc, jcol, i, j, kdeb, kfin, ki, jvec, k
    character(len=19) :: nom19
    character(len=24) :: valm
    complex(kind=8) :: czero
    aster_logical :: nonsym
    integer :: keta, iexi, ieq
    integer, pointer :: ccid(:) => null()
!     ------------------------------------------------------------------
!
!
!
    call jemarq()
    nom19=zk24(zi(lmat+1))
    valm=nom19//'.VALM'
    call jelira(valm, 'NMAXOC', nbloc)
    ASSERT(nbloc.eq.1 .or. nbloc.eq.2)
    nonsym=(nbloc.eq.2)
    czero=dcmplx(0.d0,0.d0)
    if (cumul .eq. 'ZERO') then
        do 20 i = 1, nbvect
            do 10 j = 1, neq
                xsol(j,i)=czero
 10         continue
 20     continue
    endif
!     -- VALM(1) : AU DESSUS DE LA DIAGONALE
    call jeveuo(jexnum(valm, 1), 'L', jmat1)
    if (nonsym) then
!        -- VALM(2) : AU DESSOUS DE LA DIAGONALE
        call jeveuo(jexnum(valm, 2), 'L', jmat2)
    else
        jmat2=jmat1
    endif
!
!
    do 60 jvec = 1, nbvect
        do 30 k = 1, neq
            vectmp(k)=vect(k,jvec)
 30     continue
!        -- LES LAGRANGE DOIVENT ETRE MIS A L'ECHELLE AVANT LA
!           MULTIPLICATION :
        if (prepos) call mcconl('DIVI', lmat, 0, 'C', vectmp,&
                                1)
!
!        -- PREMIERE LIGNE
        xsol(1,jvec)=xsol(1,jvec)+zr(jmat1-1+1)*vectmp(1)
!
!        -- LIGNES SUIVANTES
        do 50 i = 2, neq
            kdeb=smdi(i-1)+1
            kfin=smdi(i)-1
            do 40 ki = kdeb, kfin
                jcol=smhc(ki)
                xsol(jcol,jvec)=xsol(jcol,jvec)+zr(jmat1-1+ki)*vectmp(&
                i)
                xsol(i,jvec)=xsol(i,jvec)+zr(jmat2-1+ki)*vectmp(jcol)
 40         continue
            xsol(i,jvec)=xsol(i,jvec)+zr(jmat1+kfin)*vectmp(i)
 50     continue
        if (prepos) call mcconl('DIVI', lmat, 0, 'C', xsol(1, jvec),&
                                1)
 60 end do
!
!
!     -- POUR LES DDLS ELIMINES PAR AFFE_CHAR_CINE, ON NE PEUT PAS
!        CALCULER F=K*U. CES DDLS SONT MIS A ZERO.
!     -------------------------------------------------------------
    call jeexin(nom19//'.CCID', iexi)
    if (iexi .ne. 0) then
        call jeveuo(nom19//'.CCID', 'L', vi=ccid)
        do 110 jvec = 1, nbvect
            do 111 ieq = 1, neq
                keta=ccid(ieq)
                ASSERT(keta.eq.1 .or. keta.eq.0)
                if (keta .eq. 1) xsol(ieq,jvec)=dcmplx(0.d0,0.d0)
111         continue
110     continue
    endif
!
!
    call jedema()
end subroutine
