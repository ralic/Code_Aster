subroutine op0033()
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jean-michel.proix at edf.fr
!
    implicit none
!
! ----------------------------------------------------------------------
!  OPERATEUR    CALC_POINT_MAT
! ----------------------------------------------------------------------
!
!
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/dierre.h"
#include "asterfort/diinst.h"
#include "asterfort/fointe.h"
#include "asterfort/getvid.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveut.h"
#include "asterfort/lcdetf.h"
#include "asterfort/matinv.h"
#include "asterfort/mgauss.h"
#include "asterfort/nmadat.h"
#include "asterfort/nmcomp.h"
#include "asterfort/nmcpel.h"
#include "asterfort/nmcrcv.h"
#include "asterfort/nmdcin.h"
#include "asterfort/nmfinp.h"
#include "asterfort/pmactn.h"
#include "asterfort/pmat.h"
#include "asterfort/pmconv.h"
#include "asterfort/pmdorc.h"
#include "asterfort/pmdrdy.h"
#include "asterfort/pmimpr.h"
#include "asterfort/pminit.h"
#include "asterfort/pmmaco.h"
#include "asterfort/pmsta1.h"
#include "asterfort/pmstab.h"
#include "asterfort/pmvtgt.h"
#include "asterfort/r8inir.h"
#include "asterfort/tnsvec.h"
#include "asterfort/utbtab.h"
#include "asterfort/vrcinp.h"
#include "asterfort/wkvect.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
#include "blas/dscal.h"
    integer :: ndim, iret, nbmat, nbvari, nbpar, i, incela, ier
    integer :: imate, kpg, ksp, iter, pred, ncmp, imptgt, nbvrcm
    integer :: ntamax, matrel, irota, defimp, liccvg(5)
    integer :: indimp(9), numins, actite, action, itgt, iforta
!     NOMBRE MAXI DE COLONNES DANS UNE TABLE 9999 (CF D4.02.05)
    parameter    ( ntamax = 9999 )
    integer :: ncmpma, dimaki, dimanv, igrad, nbvita
!    DIMAKI = DIMENSION MAX DE LA LISTE DES RELATIONS KIT
    parameter    (dimaki=9)
!    DIMANV = DIMENSION MAX DE LA LISTE DU NOMBRE DE VAR INT EN THM
    parameter    (dimanv=4)
    parameter    (ncmpma=7+dimaki+dimanv)
!     nombre de variables de commande maxi
    parameter    (nbvrcm=100)
    character(len=4) :: fami, cargau
    character(len=8) :: typmod(2), mater(30), table, fonimp(9), typpar(ntamax)
    character(len=16) :: option, compor(ncmpma), nompar(ntamax), opt2
    character(len=19) :: codi, sddisc, k19b, sdcrit
    character(len=24) :: sderro
    real(kind=8) :: instam, instap, ang(7), r8b, carcri(18), fem(9)
    real(kind=8) :: deps(9), sigm(6), sigp(6), epsm(9), eps(9), vr(ntamax)
    real(kind=8) :: valimp(9), r(12), rini(12), dy(12), ddy(12), y(12), rac2
    real(kind=8) :: dsidep(6, 9), drdy(12, 12), kel(6, 6), cimpo(6, 12), ym(12)
    real(kind=8) :: work(10), sdeps(6), ssigp(6), smatr(36), r1(12)
    real(kind=8) :: matper(36), varia(2*36), epsilo, pgl(3, 3), vimp33(3, 3)
    real(kind=8) :: vimp2(3, 3), coef, parcri(7), jm, jp, jd, rbid(1), coefextra
    aster_logical :: finpas, itemax, conver
    character(len=19) :: nomvi
    character(len=19) :: vim, vip, vim2, svip
    integer :: lvim, lvip, lvim2, lsvip, lnomvi
!
    data sddisc  /'&&OP0033.SDDISC'/
    data sdcrit  /'&&OP0033.SDCRIT'/
    data sderro  /'&&OP0033.ERRE.'/
!
    data vim     /'&&OP0033.VIM'/
    data vip     /'&&OP0033.VIP'/
    data svip    /'&&OP0033.SVIP'/
    data vim2    /'&&OP0033.VIM2'/
    data nomvi   /'&&OP0033.NOMVI'/
!
! ======================================================================
! --- RECUPERATION DES ARGUMENTS  DE LA COMMANDE
! ======================================================================
    call infmaj()
    call jemarq()
    ndim=3
    rac2=sqrt(2.d0)
    fami='PMAT'
    kpg=1
    ksp=1
    k19b=' '
    iter = 0
    action=1
    finpas=.false.
    itemax=.false.
    do 10 i = 1, 5
        liccvg(i)=0
 10 end do
!
!     RECUPERATION DES OPTIONS DEMANDEES
!     ----------------------------------
    call getvid(' ', 'MATER', nbval=6, vect=mater, nbret=nbmat)
!
!     RECUPERATION DU COMPORTEMENT
!     ----------------------------
    call pmdorc(compor, carcri, nbvari, incela)
!
    call wkvect(vim, 'V V R', nbvari, lvim)
    call wkvect(vip, 'V V R', nbvari, lvip)
    call wkvect(svip, 'V V R', nbvari, lsvip)
    call wkvect(vim2, 'V V R', nbvari, lvim2)
    call wkvect(nomvi, 'V V K8', nbvari, lnomvi)
!
!
!-------------------------------------------------------------------
!     LECTURE MATERIAU ET CODAGE
!-------------------------------------------------------------------
    call r8inir(10, 0.d0, work, 1)
    call pmmaco(mater, nbmat, codi)
    call jeveut(codi//'.CODI', 'L', imate)
!-------------------------------------------------------------------
!
!     GESTION DES VARIABLES DE COMMANDE
!
    call vrcinp(nbvrcm, 1, 0.d0, 0.d0)
!
!
!     INITIALISATIONS SD
    call pminit(imate, nbvari, ndim, typmod, table,&
                nbpar, iforta, nompar, typpar, ang,&
                pgl, irota, epsm, sigm, zr(lvim),&
                zr(lvip), vr, defimp, coef, indimp,&
                fonimp, cimpo, kel, sddisc, parcri,&
                pred, matrel, imptgt, option, zk8(lnomvi),&
                nbvita, nbvrcm, sderro)
    call r8inir(54, 0.d0, dsidep, 1)
!
! --- CREATION DE LA SD POUR ARCHIVAGE DES INFORMATIONS DE CONVERGENCE
!
    call nmcrcv(sdcrit)
    numins=1
!
!==================================
!     BOUCLE SUR lES INSTANTS
!==================================
!
200 continue
    do 11 i = 1, 5
        liccvg(i)=0
 11 continue
!        RECUPERATION DU NUMERO D'ORDRE ET DE L'INSTANT COURANTS
!        DECOUPE INITIALE DU PAS DE TEMPS
!
    call nmdcin(sddisc, numins)
    instam = diinst(sddisc, numins-1)
    instap = diinst(sddisc, numins )
!        CALCUL DES VARIABLES DE COMMANDE
    call vrcinp(nbvrcm, 2, instam, instap)
!
    if (defimp .lt. 2) then
        igrad=0
!            VALEURS IMPOSEES DE CONTRAINTES OU DEFORMATIONS
        do 20 i = 1, 6
            call fointe('F', fonimp(i), 1, ['INST'], [instap],&
                        valimp(i), ier)
!               NORMALISATION DES TERMES EN CONTRAINTES
            if (indimp(i) .eq. 0) valimp(i)=valimp(i)/coef
 20     continue
!            NORMALEMENT DEJA VERIFIE PAR SIMU_POINT_MAT_OPS
        ASSERT(compor(3).eq.'PETIT')
    else if (defimp.eq.2) then
        igrad=1
!           VALEURS IMPOSEES DE GRADIENTS F
        do 21 i = 1, 9
            call fointe('F', fonimp(i), 1, ['INST'], [instap],&
                        valimp(i), ier)
 21     continue
    endif
!
    if (irota .eq. 1) then
        call tnsvec(6, ndim, vimp33, valimp, 1.0d0)
        call utbtab('ZERO', 3, 3, vimp33, pgl,&
                    work, vimp2)
        call tnsvec(3, ndim, vimp2, valimp, 1.0d0)
    endif
!        CISAILLEMENTS*SQRT(2) POUR NMCOMP
    if (defimp .lt. 2) then
        call dscal(3, rac2, valimp(4), 1)
    endif
!
!        6 CMP DE EPSI OU 9 CMP DE GRAD DONNEES : PAS BESOIN DE NEWTON
    if ((defimp.ge.1) .and. (abs(carcri(2)).lt.0.1d0)) then
        opt2='RAPH_MECA'
        if (imptgt .eq. 1) opt2='FULL_MECA'
        if (defimp .eq. 1) then
            ncmp=6
            do 30 i = 1, ncmp
                deps(i)=valimp(i)-epsm(i)
 30         continue
        else if (defimp.eq.2) then
            ncmp=9
            call matinv('S', 3, epsm, fem, jm)
            call pmat(3, valimp, fem, deps)
            call lcdetf(3, deps, jd)
            jp = jm*jd
        endif
        if (incela .eq. 1) then
            call dcopy(nbvari, zr(lvim), 1, zr(lvim2), 1)
            call nmcomp(fami, kpg, ksp, ndim, typmod,&
                        imate, compor, carcri, instam, instap,&
                        ncmp, epsm, deps, 6, sigm,&
                        zr(lvim2), opt2, ang, 10, work,&
                        sigp, zr(lvip), 6*ncmp, dsidep, 1,&
                        rbid, iret)
            if (compor(3) .eq. 'SIMO_MIEHE') then
                call dscal(2*ndim, 1.d0/jp, sigp, 1)
            endif
        else if (incela.eq.2) then
            call dcopy(ncmp, epsm, 1, eps, 1)
            call daxpy(ncmp, 1.d0, deps, 1, eps,&
                       1)
            call nmcpel(fami, kpg, 1, '+', ndim,&
                        typmod, ang, imate, compor, carcri,&
                        option, eps, sigp, zr(lvip), dsidep,&
                        iret)
        else
            ASSERT(.false.)
        endif
        call pmimpr(0, instap, indimp, fonimp, valimp,&
                    0, epsm, sigm, zr(lvim), nbvari,&
                    r, r8b, r8b)
        if (iret .ne. 0) then
            liccvg(2) = 1
            goto 500
        endif
        goto 550
    endif
!
!        INITIALISATION DE L'ALGO DE NEWTON
!
    call dcopy(6, sigm, 1, ym, 1)
    call dscal(6, 1.d0/coef, ym, 1)
    call dcopy(6, epsm, 1, ym(7), 1)
!
    if (pred .eq. 1) then
        call r8inir(12, 0.d0, dy, 1)
        call r8inir(6, 0.d0, deps, 1)
        opt2='RIGI_MECA_TANG'
        call dcopy(nbvari, zr(lvim), 1, zr(lsvip), 1)
        if (incela .eq. 1) then
            call nmcomp(fami, kpg, ksp, ndim, typmod,&
                        imate, compor, carcri, instam, instap,&
                        6, epsm, deps, 6, sigm,&
                        zr(lsvip), opt2, ang, 10, work,&
                        ssigp, zr(lsvip), 36, dsidep, 1,&
                        rbid, iret)
        else if (incela.eq.2) then
            call nmcpel(fami, kpg, 1, '+', ndim,&
                        typmod, ang, imate, compor, carcri,&
                        option, epsm, sigp, zr(lvip), dsidep,&
                        iret)
        endif
        if (iret .ne. 0) then
            pred=0
        else
            call pmdrdy(dsidep, coef, cimpo, valimp, ym,&
                        sigm, r, drdy)
        endif
    else if ((pred .eq. 0).or.((pred.eq.-1).and.(numins.eq.1))) then
        call r8inir(12, 0.d0, dy, 1)
        call r8inir(6, 0.d0, deps, 1)
        call pmdrdy(kel, coef, cimpo, valimp, ym,&
                    sigm, r, drdy)
    endif
!        SAUVEGARDE DE R(DY0) POUR TEST DE CONVERGENCE
    call dcopy(12, r, 1, rini, 1)
    call pmimpr(0, instap, indimp, fonimp, valimp,&
                0, epsm, sigm, zr(lvim), nbvari,&
                r, r8b, r8b)
!
    iter = 0
!
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!           ITERATIONS DE NEWTON
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!
300 continue
!
    iter = iter + 1
!
    if ((iter.eq.1) .and. (pred.eq.-1) .and. (numins.gt.1)) then
!   prediction='extrapole'
        coefextra = (instap-instam)/(instam-diinst(sddisc, numins-2))
!       dy = dy * (ti - ti-1)/(ti-1 - ti-2)
        call dscal(12, coefextra, dy, 1)
    else
!
        call dcopy(12, r, 1, ddy, 1)
!
!      RESOLUTION DE DRDY*DDY = - R(Y)  CARGAU = 'NCSP'
        cargau = 'NCWP'
        call mgauss(cargau, drdy, ddy, 12, 12,&
                    1, r8b, iret)
        if (iret .ne. 0) then
            liccvg(5) = 1
            conver = .false.
            goto 500
        endif
!
!      REACTUALISATION DE DY = DY + DDY
        call daxpy(12, 1.d0, ddy, 1, dy,&
                   1)
!
    endif
!
    call dcopy(6, dy(7), 1, deps, 1)
!
!           POUR LE CALCUL DE LA MATRICE TANGENTE PAR PERTURBATION
1000 continue
!
!           CALCUL DU RESIDU
    liccvg(2) = 0
    if (incela .eq. 1) then
        call dcopy(nbvari, zr(lvim), 1, zr(lvim2), 1)
        call nmcomp(fami, kpg, ksp, ndim, typmod,&
                    imate, compor, carcri, instam, instap,&
                    6, epsm, deps, 6, sigm,&
                    zr(lvim2), option, ang, 10, work,&
                    sigp, zr(lvip), 36, dsidep, 1,&
                    rbid, iret)
    else if (incela.eq.2) then
        call dcopy(6, epsm, 1, eps, 1)
        call daxpy(6, 1.d0, deps, 1, eps,&
                   1)
        call nmcpel(fami, kpg, 1, '+', ndim,&
                    typmod, ang, imate, compor, carcri,&
                    option, eps, sigp, zr(lvip), dsidep,&
                    iret)
    endif
!
    call pmimpr(1, instap, indimp, fonimp, valimp,&
                iter, deps, sigp, zr(lvip), nbvari,&
                r, r8b, r8b)
    if (iret .ne. 0) then
        conver = .false.
        liccvg(2) = 1
        goto 500
    endif
!
!           CALCUL EVENTUEL DE LA MATRICE TGTE PAR PERTURBATION
    call pmvtgt(option, carcri, deps, sigp, zr(lvip),&
                nbvari, epsilo, varia, matper, dsidep,&
                smatr, sdeps, ssigp, zr(lsvip), itgt)
    if (itgt .ne. 0) then
        goto 1000
    endif
!
    call dcopy(12, ym, 1, y, 1)
    call daxpy(12, 1.d0, dy, 1, y,&
               1)
    if (matrel .eq. 1) then
        call pmdrdy(kel, coef, cimpo, valimp, y,&
                    sigp, r, drdy)
    else
        call pmdrdy(dsidep, coef, cimpo, valimp, y,&
                    sigp, r, drdy)
    endif
!
!           VERIFICATION DE LA CONVERGENCE EN DY  ET RE-INTEGRATION ?
    call pmconv(r, rini, r1, instap, sigp,&
                coef, iter, indimp, parcri, conver,&
                itemax)
!
!           ENREGISTRE LES RESIDUS A CETTE ITERATION
    call dierre(sddisc, sdcrit, iter)
!
!           VERIFICATION DES EVENT-DRIVEN
500 continue
    call pmsta1(sigm, sigp, deps, zr(lvim), zr(lvip),&
                nbvari, nbvita, iforta, nbpar, nompar,&
                vr, igrad, typpar, zk8(lnomvi), sddisc,&
                liccvg, itemax, conver, actite)
!
!           ON CONTINUE NEWTON
    if (actite .eq. 2) goto 300
!
! ======================================================================
!     FIN DES ITERATIONS DE NEWTON
! ======================================================================
!
!        GESTION DE LA DECOUPE DU PAS DE TEMPS
!        EN L'ABSENCE DE CONVERGENCE ON CHERCHE A SUBDIVISER LE PAS
!        DE TEMPS SI L'UTILISATEUR A FAIT LA DEMANDE
    call pmactn(sddisc, parcri, iter, numins, itemax,&
                sderro, liccvg, actite, action)
!
! ---    ACTION
!          0 ARRET DU CALCUL
!          1 NOUVEAU PAS DE TEMPS
!          2 ON FAIT DES ITERATIONS DE NEWTON EN PLUS
!          3 ON FINIT LE PAS DE TEMPS
    if (action .eq. 1) then
        goto 600
    else if (action.eq.2) then
        goto 300
    else if (action.eq.3) then
        goto 550
    else if (action.eq.0) then
        goto 550
    endif
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!
550 continue
!
!        ---------------------------------------------------------------
!        CONVERGENCE => MISE A JOUR DE SIGM,ZR(LVIM), TABLE
!        ---------------------------------------------------------------
!
!        ADAPTATION DU NOUVEAU PAS DE TEMPS
!        PAS DE GESTION DE DELTA_GRANDEUR ACTUELLEMENT
    call nmfinp(sddisc, numins, finpas)
    if (.not.finpas) call nmadat(sddisc, numins, iter, k19b)
    numins=numins+1
!        STOCKAGE EFFECTIF DU RESULTAT DANS LA TABLE
    call pmstab(sigm, sigp, epsm, deps, nbvari,&
                zr(lvim), zr(lvip), iforta, instam, instap,&
                iter, nbpar, nompar, table, vr,&
                igrad, valimp, imptgt, dsidep, zk8(lnomvi),&
                nbvita)
    call pmimpr(2, instap, indimp, fonimp, valimp,&
                iter, deps, sigp, zr(lvip), nbvari,&
                r, r8b, r8b)
!
600 continue
!
! --- DERNIER INSTANT DE CALCUL ? -> ON SORT DE STAT_NON_LINE
!
    if (finpas .or. (action.eq.0)) then
        goto 900
    endif
    goto 200
!==================================
!     FIN BOUCLE SUR LES INSTANTS
!==================================
!
900 continue
!
!     GESTION DES VARIABLES DE COMMANDE
    call vrcinp(nbvrcm, 0, instam, instap)
!
!
!     DESTRUCTION DE lA FONCTION F0 NULLE
    call detrsd('FONCTION', '&&CPM_F0')
!
    call jedema()
end subroutine
