subroutine mrmmvr(cumul, lmat, smdi, smhc, lmatd,&
                  neq, neql, vect, xsol, nbvect,&
                  vectmp, prepos)
! aslint: disable=W1304
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mrconl.h"
    character(len=*) :: cumul
    integer(kind=4) :: smhc(*)
    integer :: smdi(*), neq, nbvect, neql, lmat
    real(kind=8) :: vect(neq, nbvect), xsol(neq, nbvect), vectmp(neq)
    logical :: lmatd, prepos
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
! IN  CUMUL  : K4 :
!              / 'ZERO' : XSOL =        MAT*VECT
!              / 'CUMU' : XSOL = XSOL + MAT*VECT
!     ------------------------------------------------------------------
!
!
!
    real(kind=8) :: zero
    character(len=14) :: numddl
    character(len=19) :: nom19
    character(len=24) :: valm, refa
    integer :: kfin, jvalms, jvalmi, jvec, ki, kdeb, nbloc
    integer :: ilig, jcol, jrefa,  iligg, jcolg, numglo, k
    integer :: keta, iexi,  ieq
    logical :: nonsym
    integer, pointer :: nulg(:) => null()
    integer, pointer :: ccid(:) => null()
!     ------------------------------------------------------------------
!
!
!
    call jemarq()
    nom19=zk24(zi(lmat+1))(1:19)
!
    valm=nom19//'.VALM'
    call jelira(valm, 'NMAXOC', nbloc)
    ASSERT(nbloc.eq.1 .or. nbloc.eq.2)
    nonsym=(nbloc.eq.2)
!
    zero=0.d0
    ASSERT(cumul.eq.'ZERO' .or. cumul.eq.'CUMU')
    if (cumul .eq. 'ZERO') then
        do 20 jvec = 1, nbvect
            do 10 ilig = 1, neq
                xsol(ilig,jvec)=zero
10          continue
20      continue
    endif
!
!
!     -- VALM(1) : AU DESSUS DE LA DIAGONALE
    call jeveuo(jexnum(valm, 1), 'L', jvalms)
    if (nonsym) then
!        -- VALM(2) : AU DESSOUS DE LA DIAGONALE
        call jeveuo(jexnum(valm, 2), 'L', jvalmi)
    else
        jvalmi=jvalms
    endif
!
!
!     -- CAS D'UNE MATRICE NON DISTRIBUEE :
!     ----------------------------------------
    if (.not.lmatd) then
        do 60 jvec = 1, nbvect
            do 30,k=1,neq
            vectmp(k)=vect(k,jvec)
30          continue
!         -- LES LAGRANGE DOIVENT ETRE MIS A L'ECHELLE AVANT LA
!            MULTIPLICATION :
            if (prepos) call mrconl('DIVI', lmat, 0, 'R', vectmp,&
                                    1)
            xsol(1,jvec)=xsol(1,jvec)+zr(jvalms-1+1)*vectmp(1)
            do 50 ilig = 2, neq
                kdeb=smdi(ilig-1)+1
                kfin=smdi(ilig)-1
!           CDIR$ IVDEP
                do 40 ki = kdeb, kfin
                    jcol=smhc(ki)
                    xsol(ilig,jvec)=xsol(ilig,jvec)+ zr(jvalmi-1+ki)*&
                    vectmp(jcol)
                    xsol(jcol,jvec)=xsol(jcol,jvec)+ zr(jvalms-1+ki)*&
                    vectmp(ilig)
40              continue
                xsol(ilig,jvec)=xsol(ilig,jvec)+zr(jvalms+kfin)*&
                vectmp(ilig)
50          continue
            if (prepos) call mrconl('DIVI', lmat, 0, 'R', xsol(1, jvec),&
                                    1)
60      continue
!
!
!     -- CAS D'UNE MATRICE DISTRIBUEE :
!     ----------------------------------------
    else
        refa=nom19//'.REFA'
        call jeveuo(refa, 'L', jrefa)
        numddl=zk24(jrefa+2-1)(1:14)
        call jeveuo(numddl//'.NUML.NULG', 'L', vi=nulg)
        do 100 jvec = 1, nbvect
            do 70,k=1,neq
            vectmp(k)=vect(k,jvec)
70          continue
            if (prepos) call mrconl('DIVI', lmat, 0, 'R', vectmp,1)
            numglo=nulg(1)
            xsol(numglo,jvec)=xsol(numglo,jvec)+ zr(jvalms-1+1)*&
            vectmp(numglo)
            do 90 ilig = 2, neql
                iligg=nulg(ilig)
                kdeb=smdi(ilig-1)+1
                kfin=smdi(ilig)-1
!           CDIR$ IVDEP
                do 80 ki = kdeb, kfin
                    jcol=smhc(ki)
                    jcolg=nulg(jcol)
                    xsol(iligg,jvec)=xsol(iligg,jvec)+ zr(jvalmi-1+ki)&
                    *vectmp(jcolg)
                    xsol(jcolg,jvec)=xsol(jcolg,jvec)+ zr(jvalms-1+ki)&
                    *vectmp(iligg)
80              continue
                xsol(iligg,jvec)=xsol(iligg,jvec)+ zr(jvalms+kfin)*&
                vectmp(iligg)
90          continue
            if (prepos) call mrconl('DIVI', lmat, 0, 'R', xsol(1, jvec),1)
100      continue
    endif
!
!
!     -- POUR LES DDLS ELIMINES PAR AFFE_CHAR_CINE, ON NE PEUT PAS
!        CALCULER F=K*U. CES DDLS SONT MIS A ZERO.
!     -------------------------------------------------------------
    call jeexin(nom19//'.CCID', iexi)
    if (iexi .ne. 0) then
        call jeveuo(nom19//'.CCID', 'L', vi=ccid)
        do jvec = 1, nbvect
            do ieq=1,neql
                if (lmatd) then
                    keta=ccid(nulg(ieq))
                else
                    keta=ccid(ieq)
                endif
                ASSERT(keta.eq.1 .or. keta.eq.0)
                if (keta .eq. 1) xsol(ieq,jvec)=0.d0
            enddo
        enddo
    endif
!
!
!
    call jedema()
end subroutine
