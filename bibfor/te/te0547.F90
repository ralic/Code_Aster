subroutine te0547(option, nomte)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/elelin.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/pipeba.h"
#include "asterfort/pipel2.h"
#include "asterfort/pipeou.h"
#include "asterfort/pipetc.h"
#include "asterc/r8vide.h"
#include "asterfort/teattr.h"
#include "asterfort/tecach.h"
#include "asterfort/tecael.h"
#include "asterfort/vecini.h"
#include "asterfort/xminte.h"
#include "asterfort/xmmsa2.h"
#include "asterfort/xmmsa4.h"
#include "asterfort/xmmsa5.h"
#include "asterfort/xmprep.h"
#include "asterfort/xmulco.h"
#include "asterfort/xteini.h"
#include "asterfort/xxlag3.h"
#include "asterfort/xxlan5.h"
    character(len=16) :: option, nomte
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
! person_in_charge: patrick.massin at edf.fr
!
! ......................................................................
!
!       CALCUL DES COEFFICIENTS A0 ET A1 POUR LE PILOTAGE PAR
!      PREDICTION ELASTIQUE POUR LES ELEMENTS X_FEM EN CONTACT
!               SOUMIS A LA LOI COHESIVE CZM_EXP_REG
!
!  OPTION : 'PILO_PRED_XLAS'
!
!  ENTREES  ---> OPTION : OPTION DE CALCUL
!           ---> NOMTE  : NOM DU TYPE ELEMENT
!
!......................................................................
!
!
!
    integer :: i, j, ibid, nfe, ib, iret, ino
    integer :: ndim, ddlc, ddls, nddl
    integer :: nno, nnos, nnom, nnof
    integer :: ipoids, ivf, idfde, jgano, ddlm
    integer :: ipoidf, ivff, idfdef, iadzi, iazk24
    integer :: npg, npgf, nptf, integ, singu
    integer :: ninter, nface, cface(30, 6)
    integer :: lact(8), nlact, ncompv
    integer :: jdonco, jlsn, jlst, igeom, jptint, vstnc(1)
    integer :: jaint, jcface, jlonch, jbasec, icopil, ictau
    integer :: idepl0, idepl1, ideplm, iddepl, jcohes, imate
    integer :: nfh, nfiss, contac, jta2(3)
    character(len=8) :: elref, typma, fpg, elc, elrefc
    character(len=16) :: enr
    real(kind=8) :: rela, lup(3), lud(3)
!
    real(kind=8) :: cohes, copilo(5), dtau, ffc(8), ffp(27)
    real(kind=8) :: mat3bd(3, 3), mat6bd(6, 6), alpha(3), cohes2(3)
    real(kind=8) :: jac, mud(3), mup(3), r3bd(3), ma3bd(3, 3)
    real(kind=8) :: nd(3), r8bid, r6bid(6), r3bid(3)
    real(kind=8) :: rr, rbid, sud(3), sud2d(2), sudd(3), sup(3)
    real(kind=8) :: sup2d(2), supp(3), tau1(3), tau2(3), vim(9)
    integer :: ifa, ipgf, isspg, mate, nnol, nvec, pla(27)
    character(len=8) :: job, champ
    aster_logical :: lbid
!......................................................................
!
!-----------------------------------------------------------------------
!     INITIALISATIONS
!-----------------------------------------------------------------------
!
    call elref1(elref)
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jvf=ivf, jdfde=idfde, jgano=jgano)
!
!     INITIALISATION DES DIMENSIONS DES DDLS X-FEM
!
    call xteini(nomte, nfh, nfe, singu, ddlc,&
                nnom, ddls, nddl, ddlm, nfiss,&
                contac)
    call tecael(iadzi, iazk24, noms=0)
    typma=zk24(iazk24-1+3+zi(iadzi-1+2)+3)
!
! --- ROUTINE SPECIFIQUE P2P1
!.
    call elelin(contac, elref, elrefc, ibid, ibid)
!
!-----------------------------------------------------------------------
!     RECUPERATION DES ENTREES / SORTIE
!-----------------------------------------------------------------------
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PDEPLMR', 'L', ideplm)
    call jevech('PDDEPLR', 'L', iddepl)
    call jevech('PDEPL0R', 'L', idepl0)
    call jevech('PDEPL1R', 'L', idepl1)
    call jevech('PDONCO', 'L', jdonco)
    call jevech('PLSN', 'L', jlsn)
    call jevech('PLST', 'L', jlst)
    call jevech('PPINTER', 'L', jptint)
    call jevech('PAINTER', 'L', jaint)
    call jevech('PCFACE', 'L', jcface)
    call jevech('PLONGCO', 'L', jlonch)
    call jevech('PBASECO', 'L', jbasec)
    call jevech('PCDTAU', 'L', ictau)
    dtau = zr(ictau)
!
    call teattr('S', 'XFEM', enr, ibid)
    if (enr(1:3) .eq. 'XHC') then
        rela = zr(jdonco-1+10)
    else
        rela=0.0d0
    endif
!
    if (rela .ne. 0.d0) then
        call jevech('PMATERC', 'L', imate)
        call jevech('PCOHES', 'L', jcohes)
        call tecach('OOO', 'PCOHES', 'L', iret, nval=3,&
                    itab=jta2)
        if(contac.eq.2) ncompv = jta2(2)/jta2(3)
        if(contac.eq.1.or.contac.eq.3) ncompv = jta2(2)
    endif
    mate = zi(imate)
!
! RECUPERATIONS DES DONNEES SUR LA TOPOLOGIE DES FACETTES
!
    ninter=zi(jlonch)
    nface=zi(jlonch-1+2)
    nptf=zi(jlonch-1+3)
!
    do i = 1, nface
        do j = 1, nptf
            cface(i,j)=zi(jcface-1+nptf*(i-1)+j)
        end do
    end do
!
! SCHEMA D'INTEGRATION NUMERIQUE ET ELEMENT DE REFERENCE DE CONTACT
! DISCUSSION VOIR BOOK IV 18/10/2004 ET BOOK VI 06/07/2005
!
    integ = nint(zr(jdonco-1+4))
    call xminte(ndim, integ, fpg)
    if (ndim .eq. 3) then
        elc='TR3'
    else if (ndim.eq.2) then
        if (contac .le. 2) then
            elc='SE2'
        else
            elc='SE3'
        endif
    endif
!
! RECUPERATION DU NOMBRE DE POINTS DE GAUSS NPGF
    call elrefe_info(elrefe=elc, fami=fpg, nno=nnof, npg=npgf, jpoids=ipoidf,&
                     jvf=ivff, jdfde=idfdef)
!
!
! LISTE DES LAMBDAS ACTIFS
!
    call xmulco(contac, ddls, ddlc, ddlm, jaint, 1,&
                ibid, vstnc, lact, .false._1, lbid,&
                ndim, nfh, 1, ninter,&
                nlact, nno, nnol, nnom, nnos,&
                pla, typma)
!
! PARAMETRES EN SORTIE
!
    call jevech('PCOPILO', 'E', icopil)
!
! SI COHESIF TYPE "MORTAR"
!
    if(contac.eq.2) then
!
        ASSERT(rela.eq.5.d0)
        call xmprep(cface, contac, elref, elrefc, elc,&
                    ffc, ffp, fpg, jaint, jbasec,&
                    jptint, 1, igeom, 1, jac,&
                    jlst, lact, nd, ndim, ninter,&
                    nlact, nno, nnos, nptf, ibid,&
                    rr, singu, tau1, tau2)
!
        do 120 ino = 1, nnol
!         INDICE DE CE POINT DE GAUSS DANS INDCO
            cohes = zr(jcohes+ncompv*(ino-1))
!
!           INITIALISATION
!
            call vecini(3, 0.d0, mup)
            call vecini(3, 0.d0, mud)
            call vecini(3, 0.d0, lup)
            call vecini(3, 0.d0, lud)
            call vecini(5, 0.d0, copilo)
!
! CODE ERREUR A UNDEF. UNE VALEUR INDIQUE UN PLANTAGE
!
            copilo(5) = r8vide()
!
!           SAUT PILOTE AU POINT DE GAUSS : SU(ETA) = SUPP + ETA * SUDD
!           COMPOSANTE PILOTEE DU SAUT SUDD
!           SAUT A T- SAUTM
!           COMPOSANTE FIXE DU SAUT A ITERATION N+1 SUPP
!
            nvec = 3
            champ = 'W'
            call xxlan5(ino, ideplm, iddepl, idepl0, lact,&
                        ndim, pla, sup, nvec, champ)
            nvec = 3
            champ = 'LAMBDA'
            call xxlan5(ino, ideplm, iddepl, idepl0, lact,&
                        ndim, pla, lup, nvec, champ)
            nvec = 1
            champ = 'W'
            call xxlan5(ino, idepl1, ibid, ib, lact,&
                        ndim, pla, sud, nvec, champ)
            nvec = 1
            champ = 'LAMBDA'
            call xxlan5(ino, idepl1, ibid, ib, lact,&
                        ndim, pla, lud, nvec, champ)
            call pipel2(mate, sup, sud, lup, lud, cohes,&
                        dtau, copilo)
!
            do 130 i = 1, 5
                zr(icopil-1+5*(ino-1)+i) = copilo(i)
130         continue
!
120      continue
!
!   SI COHESIF CLASSIQUE
!
    else if(contac.eq.1.or.contac.eq.3) then
!
        do ifa = 1, nface
!
            do ipgf = 1, npgf
!         INDICE DE CE POINT DE GAUSS DANS INDCO
                isspg = npgf*(ifa-1)+ipgf
                cohes = zr(jcohes+ncompv*(isspg-1))
                call xmprep(cface, contac, elref, elrefc, elc,&
                            ffc, ffp, fpg, jaint, jbasec,&
                            jptint, ifa, igeom, ipgf, jac,&
                            jlst, lact, nd, ndim, ninter,&
                            nlact, nno, nnos, nptf, ibid,&
                            rr, singu, tau1, tau2)
!
!           INITIALISATION
!
                call vecini(2, 0.d0, sup2d)
                call vecini(2, 0.d0, sud2d)
                call vecini(3, 0.d0, mup)
                call vecini(3, 0.d0, mud)
                call vecini(9, 0.d0, vim)
                call vecini(5, 0.d0, copilo)
!
! CODE ERREUR A UNDEF. UNE VALEUR INDIQUE UN PLANTAGE
!
                copilo(5) = r8vide()
!
!           SAUT PILOTE AU POINT DE GAUSS : SU(ETA) = SUPP + ETA * SUDD
!           COMPOSANTE PILOTEE DU SAUT SUDD
!           SAUT A T- SAUTM
!           COMPOSANTE FIXE DU SAUT A ITERATION N+1 SUPP
!
                nvec = 3
                call xmmsa4(ndim, nno, nnos, ffp, nddl,&
                            nvec, zr(ideplm), zr( iddepl), zr(idepl0), nfh,&
                            singu, rr, ddls, ddlm, supp)
                nvec = 1
                call xmmsa4(ndim, nno, nnos, ffp, nddl,&
                            nvec, zr(idepl1), [0.d0], [0.d0], nfh,&
                            singu, rr, ddls, ddlm, sudd)
!
                if (rela .eq. 1.d0 .or. rela .eq. 2.d0) then
                    job='SAUT_LOC'
                    call xmmsa2(ndim, ipgf, mate, sudd, nd,&
                                tau1, tau2, cohes2, job, r8bid,&
                                alpha, mat6bd, r6bid, mat3bd, r3bid,&
                                r3bd, ma3bd, sud)
!
                    call xmmsa2(ndim, ipgf, mate, supp, nd,&
                                tau1, tau2, cohes2, job, r8bid,&
                                alpha, mat6bd, r6bid, mat3bd, r3bid,&
                                r3bd, ma3bd, sup)
!           APPEL DU PILOTAGE PRED_ELAS SPECIFIQUE
!           A LA LOI DE COMPORTEMENT
                    if (ndim .eq. 2) then
                        sup2d(1)=sup(1)
                        sup2d(2)=sup(2)
                        sud2d(1)=sud(1)
                        sud2d(2)=sud(2)
                        call pipeba(ndim, mate, sup2d, sud2d, cohes,&
                                    dtau, copilo)
                    else if (ndim.eq.3) then
                        call pipeba(ndim, mate, sup, sud, cohes,&
                                    dtau, copilo)
                    endif
                else if (rela.eq.3.d0.or.rela.eq.4.d0) then
!
! ON RECUPERE LAMBDA FIXE PUIS PILOTE
!
                    vim(4) = cohes
                    nvec = 3
                    call xxlag3(ffc, ideplm, iddepl, idepl0, lact,&
                                ndim, nnol, pla, mup, nvec)
                    nvec = 1
                    call xxlag3(ffc, idepl1, ibid, ib, lact,&
                                ndim, nnol, pla, mud, nvec)
!
! ON RECUPERE [U] FIXE PUIS PILOTE
!
                    job='SAUT_LOC'
                    call xmmsa5(ndim, ipgf, mate, sudd, r3bid,&
                                nd, tau1, tau2, cohes2, job,&
                                rela, alpha, mat6bd, r6bid, mat3bd,&
                                sud, rbid)
                    call xmmsa5(ndim, ipgf, mate, supp, r3bid,&
                                nd, tau1, tau2, cohes2, job,&
                                rela, alpha, mat6bd, r6bid, mat3bd,&
                                sup, rbid)
!
! APPEL DU PILOTAGE PRED_ELAS SPECIFIQUE LOI DE COMPORTEMENT
!
                    if (rela .eq. 3.d0) then
                        call pipetc(mate, sup, sud, mup, mud,&
                                    vim, dtau, copilo)
                    else if (rela.eq.4.d0) then
                        call pipeou(mate, sup, sud, mup, mud,&
                                    vim, dtau, copilo)
!
                    endif
                endif
!
                do i = 1, 5
                    zr(icopil-1+5*(isspg-1)+i) = copilo(i)
                end do
!
             end do
        end do
    endif
!
end subroutine
