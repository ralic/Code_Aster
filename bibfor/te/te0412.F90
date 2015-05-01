subroutine te0412(option, nomte)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterfort/coqrep.h"
#include "asterfort/cosiro.h"
#include "asterfort/dkqbf.h"
#include "asterfort/dkqedg.h"
#include "asterfort/dktbf.h"
#include "asterfort/dktedg.h"
#include "asterfort/dsqedg.h"
#include "asterfort/dstedg.h"
#include "asterfort/dxeffi.h"
#include "asterfort/dxefro.h"
#include "asterfort/dxmate.h"
#include "asterfort/dxqbm.h"
#include "asterfort/dxqpgl.h"
#include "asterfort/dxtbm.h"
#include "asterfort/dxtpgl.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/gquad4.h"
#include "asterfort/gtria3.h"
#include "asterfort/jevech.h"
#include "asterfort/jquad4.h"
#include "asterfort/pmrvec.h"
#include "asterfort/q4gedg.h"
#include "asterfort/r8inir.h"
#include "asterfort/t3gedg.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "asterfort/utpvgl.h"
    character(len=16) :: option, nomte
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! FONCTIONS REALISEES:
!
!      CALCUL DE LA DENSITE D'ENERGIE POTENTIELLE THERMOELASTIQUE
!      A L'EQUILIBRE POUR LES ELEMENTS :
!             - LINEAIRE      : DKT, DST, Q4GG, DKTG ET Q4GG
!             - NON-LINEAIRE  : DKT, DKTG ET Q4GG
!      .SOIT AUX POINTS D'INTEGRATION : OPTION 'ENEL_ELGA'
!      .SOIT AUX NOEUDS               : OPTION 'ENEL_ELNO'
!      .SOIT L INTEGRALE PAR ELEMENT  : OPTION 'ENEL_ELEM'
!
!      OPTIONS : 'ENEL_ELGA'
!                'ENEL_ELEM'
!
! ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
    integer :: nnomx
    parameter (nnomx=4)
    integer :: nbsm, nbsig
    parameter (nbsm=3)
    integer :: npgmx
    parameter (npgmx=4)
!
    real(kind=8) :: pgl(3, 3)
    real(kind=8) :: eps(3), khi(3), gam(2)
    real(kind=8) :: bf(3, 3*nnomx), bm(3, 2*nnomx), um(2, nnomx), uf(3, nnomx)
    real(kind=8) :: ul(6, nnomx), qsi, eta, xyzl(3, 4), jacob(5), poids
    real(kind=8) :: cara(25)
    real(kind=8) :: nmm(nbsm), mff(nbsm)
    real(kind=8) :: enelm(npgmx), enelf(npgmx)
    real(kind=8) :: enelt(npgmx), enelc(npgmx), enemf(npgmx)
    real(kind=8) :: ent, enm, enf, enc, enmf
    real(kind=8) :: effint(32), effort(32), degpg(32)
    real(kind=8) :: alpha, beta
    real(kind=8) :: t2iu(4), t2ui(4), c, s
    real(kind=8) :: dmeps(3), dfkhi(3), dcgam(3)
    real(kind=8) :: df(9), dm(9), dmf(9), dc(4), dci(4)
    real(kind=8) :: dmc(3, 2), dfc(3, 2)
    real(kind=8) :: t1ve(9)
!
!
    integer :: ndim, nno, nnoel, npg, ipoids, icoopg, ivf, idfdx, idfd2, jgano
    integer :: jgeom, ipg, ino, jdepm, isig, jsig, idener, iret
    integer :: icompo, icontp, jvari, nbvar, ivpg
    integer :: multic
    integer :: jcara
!
    character(len=16) :: valk(3), optio2
    aster_logical :: dkq, dkg, lkit, coupmf
!
    nbsig = 6
    if (nomte .eq. 'MEDKQU4 ' .or. nomte .eq. 'MEDSQU4 ' .or. nomte .eq. 'MEQ4QU4 ') then
        dkq = .true.
        dkg = .false.
    else if (nomte.eq.'MEDKQG4 ' .or. nomte.eq.'MEQ4GG4') then
        dkq = .true.
        dkg = .true.
        nbsig = 8
    else if (nomte.eq.'MEDKTR3 ' .or. nomte.eq.'MEDSTR3 ' .or. nomte .eq.'MET3TR3 ') then
        dkq = .false.
        dkg = .false.
    else if (nomte.eq.'MEDKTG3 ' .or. nomte.eq.'MET3GG3 ') then
        dkq = .false.
        dkg = .true.
        nbsig = 8
    else
        call utmess('F', 'ELEMENTS_34', sk=nomte)
    endif
!
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnoel, npg=npg,&
                     jpoids=ipoids, jcoopg=icoopg, jvf=ivf, jdfde=idfdx, jdfd2=idfd2,&
                     jgano=jgano)
!
    call jevech('PGEOMER', 'L', jgeom)
!
    if (nno .eq. 3) then
        call dxtpgl(zr(jgeom), pgl)
    else if (nno.eq.4) then
        call dxqpgl(zr(jgeom), pgl, 'S', iret)
    endif
!
    call utpvgl(nno, 3, pgl, zr(jgeom), xyzl)
!
    if (dkq) then
        call gquad4(xyzl, cara)
    else
        call gtria3(xyzl, cara)
    endif
!
! --- INITIALISATION
!
    call r8inir(npgmx, 0.d0, enelt, 1)
    call r8inir(npgmx, 0.d0, enelm, 1)
    call r8inir(npgmx, 0.d0, enelf, 1)
    call r8inir(npgmx, 0.d0, enelc, 1)
    call r8inir(npgmx, 0.d0, enemf, 1)
    ent = 0.0d0
    enm = 0.0d0
    enf = 0.0d0
    enc = 0.0d0
    enmf= 0.0d0
!
! - ON REGARDE SI ON EST EN LINEAIRE OU ENN NON-LINEAIRE
!
    call tecach('NNN', 'PCOMPOR', 'L', iret, iad=icompo)
!
    if (iret .eq. 0) then
!
        lkit = zk16(icompo)(1:7).eq.'KIT_DDI'
!
        if (zk16(icompo)(1:4) .eq. 'ELAS' .or. zk16(icompo)(1:4) .eq. 'ENDO' .or.&
            zk16(icompo)(1:6) .eq. 'MAZARS' .or. zk16(icompo)(1:7) .eq. 'GLRC_DM' .or.&
            zk16(icompo)(1:11) .eq. 'GLRC_DAMAGE' .or.&
            (lkit .and. zk16(icompo+7)(1:7) .eq.'GLRC_DM')) then
!
            if (option .eq. 'ENEL_ELGA') then
                call jevech('PDEPLAR', 'L', jdepm)
                if (.not.dkg) then
! ---     PASSAGE DES CONTRAINTES DANS LE REPERE INTRINSEQUE :
                    call cosiro(nomte, 'PCONTRR', 'L', 'UI', 'G',&
                                icontp, 'S')
                else
                    call jevech('PCONTRR', 'L', icontp)
                endif
            else if (option.eq.'ENEL_ELEM') then
                call jevech('PDEPLR', 'L', jdepm)
                if (.not.dkg) then
! ---     PASSAGE DES CONTRAINTES DANS LE REPERE INTRINSEQUE :
                    call cosiro(nomte, 'PCONTPR', 'L', 'UI', 'G',&
                                icontp, 'S')
                else
                    call jevech('PCONTPR', 'L', icontp)
                endif
            endif
!
!
            if (dkg .and.&
                (&
                lkit .or. zk16(icompo)(1:11) .eq. 'GLRC_DAMAGE' .or. zk16(icompo)(1:4) .eq.&
                'ELAS'&
                )) then
                if (option .eq. 'ENEL_ELGA') then
                    call jevech('PVARIGR', 'L', jvari)
                else if (option.eq.'ENEL_ELEM') then
                    call jevech('PVARIPR', 'L', jvari)
                endif
            endif
            if ((.not. lkit) .or. (.not. dkg)) then
                call utpvgl(nno, 6, pgl, zr(jdepm), ul)
!
!       -- PARTITION DU DEPLACEMENT EN MEMBRANE/FLEXION :
!       -------------------------------------------------
                do ino = 1, nnoel
                    um(1,ino) = ul(1,ino)
                    um(2,ino) = ul(2,ino)
                    uf(1,ino) = ul(3,ino)
                    uf(2,ino) = ul(5,ino)
                    uf(3,ino) = -ul(4,ino)
                end do
            endif
!
!     -- CALCUL DES CONTRAINTES GENERALISEES :
!     -------------------------------------------------
            if (dkg) then
                do ipg = 1, npg
                    do isig = 1, nbsig
                        effort((ipg-1)*nbsig + isig) = zr( icontp-1 + (ipg-1 )*8 + isig )
                    end do
                end do
! --- CALCUL DES MATRICES DE CHANGEMENT DE REPERES
!
!     T2UI : LA MATRICE DE PASSAGE (2X2) : UTILISATEUR -> INTRINSEQUE
!     T2IU : LA MATRICE DE PASSAGE (2X2) : INTRINSEQUE -> UTILISATEUR
!
                call jevech('PCACOQU', 'L', jcara)
                alpha = zr(jcara+1) * r8dgrd()
                beta = zr(jcara+2) * r8dgrd()
                call coqrep(pgl, alpha, beta, t2iu, t2ui,&
                            c, s)
!
! --- PASSAGE DU VECTEUR DES EFFORTS GENERALISES AUX POINTS
! --- D'INTEGRATION DU REPERE LOCAL AU REPERE INTRINSEQUE
!
                call dxefro(npg, t2ui, effort, effint)
            else
                call dxeffi(option, nomte, pgl, zr(icontp), nbsig,&
                            effint)
            endif
!
! ---- BOUCLE SUR LES POINTS D'INTEGRATION :
!      ===================================
            do ipg = 1, npg
!
                qsi = zr(icoopg-1+ndim*(ipg-1)+1)
                eta = zr(icoopg-1+ndim*(ipg-1)+2)
                if (dkq) then
                    call jquad4(xyzl, qsi, eta, jacob)
                    poids = zr(ipoids+ipg-1)*jacob(1)
                    call dxqbm(qsi, eta, jacob(2), bm)
                    call dkqbf(qsi, eta, jacob(2), cara, bf)
                else
                    poids = zr(ipoids+ipg-1)*cara(7)
                    call dxtbm(cara(9), bm)
                    call dktbf(qsi, eta, cara, bf)
                endif
!
                if (dkg .and. lkit) then
                    read (zk16(icompo-1+2),'(I16)') nbvar
                    ivpg = jvari + (ipg-1)*nbvar + 13
                    do isig = 1, nbsm
                        eps(isig) = zr(ivpg + isig )
                        khi(isig) = zr(ivpg + isig + 3)
                    end do
                else
!
!         -- CALCUL DE EPS, KHI :
!         -----------------------------------
                    call pmrvec('ZERO', 3, 2*nnoel, bm, um,&
                                eps)
                    call pmrvec('ZERO', 3, 3*nnoel, bf, uf,&
                                khi)
!
                    if (zk16(icompo)(1:11) .eq. 'GLRC_DAMAGE') then
                        read (zk16(icompo-1+2),'(I16)') nbvar
                        ivpg = jvari + (ipg-1)*nbvar - 1
                        do isig = 1, nbsm
                            eps(isig) = eps(isig) - zr(ivpg + isig )
                            khi(isig) = khi(isig) - zr(ivpg + isig + 3)
                        end do
                    endif
                endif
!
!  --    CALCUL DE LA DENSITE D'ENERGIE POTENTIELLE ELASTIQUE :
!        ==========================================================
                if ((option.eq.'ENEL_ELGA') .or. ( option.eq.'ENEL_ELEM')) then
!
!  --      DENSITE D'ENERGIE POTENTIELLE ELASTIQUE AU POINT
!  --      D'INTEGRATION COURANT
!          ---------------------
                    call r8inir(nbsm, 0.d0, nmm, 1)
                    call r8inir(nbsm, 0.d0, mff, 1)
!
                    do isig = 1, nbsm
                        nmm(isig) = effint((ipg-1)*nbsig + isig)
                        mff(isig) = effint((ipg-1)*nbsig + isig +3)
                    end do
!
                    do jsig = 1, nbsm
                        enelm(ipg) = enelm(ipg) + 0.5d0*nmm(jsig)*eps(jsig)
                        enelf(ipg) = enelf(ipg) + 0.5d0*mff(jsig)*khi(jsig)
                    end do
                    enelt(ipg) = enelm(ipg) + enelf(ipg)
!
                    enm = enm + enelm(ipg)*poids
                    enf = enf + enelf(ipg)*poids
                    ent = ent + enelt(ipg)*poids
                endif
            end do
        endif
!
! --- CALCUL DES OPTIONS ENEL_ELGA ELEM_ELEM DANS LE CAS LINEAIRE
!     POUR LES ELEMENTS DKT, DST, Q4G, DKTG ET Q4GG
!
    else
!
        if (option .eq. 'ENEL_ELGA') then
            call jevech('PDEPLAR', 'L', jdepm)
        else if (option.eq.'ENEL_ELEM') then
            call jevech('PDEPLR', 'L', jdepm)
        endif
!
        call utpvgl(nno, 6, pgl, zr(jdepm), ul)
!
        call dxmate('RIGI', df, dm, dmf, dc,&
                    dci, dmc, dfc, nno, pgl,&
                    multic, coupmf, t2iu, t2ui, t1ve)
!
!     -- CALCUL DES DEFORMATIONS GENERALISEES AUX POINTS DE GAUSS
!     -----------------------------------------------------------
        optio2='DEGE_ELGA'
        if (nomte .eq. 'MEDKTR3' .or. nomte .eq. 'MEDKTG3') then
            call dktedg(xyzl, optio2, pgl, ul, degpg,&
                        multic)
        else if (nomte.eq.'MEDSTR3') then
            call dstedg(xyzl, optio2, pgl, ul, degpg)
        else if (nomte.eq.'MEDKQU4' .or. nomte.eq.'MEDKQG4') then
            call dkqedg(xyzl, optio2, pgl, ul, degpg)
        else if (nomte.eq.'MEDSQU4') then
            call dsqedg(xyzl, optio2, pgl, ul, degpg)
        else if (nomte.eq.'MEQ4QU4'.or. nomte.eq.'MEQ4GG4') then
            call q4gedg(xyzl, optio2, pgl, ul, degpg)
        else if (nomte.eq.'MET3TR3'.or. nomte.eq.'MET3GG3') then
            call t3gedg(xyzl, optio2, pgl, ul, degpg)
        endif
!
! ---- BOUCLE SUR LES POINTS D'INTEGRATION :
!      ===================================
        do ipg = 1, npg
!
            qsi = zr(icoopg-1+ndim*(ipg-1)+1)
            eta = zr(icoopg-1+ndim*(ipg-1)+2)
            if (dkq) then
                call jquad4(xyzl, qsi, eta, jacob)
                poids = zr(ipoids+ipg-1)*jacob(1)
            else
                poids = zr(ipoids+ipg-1)*cara(7)
            endif
!
!  --    CALCUL DE LA DENSITE D'ENERGIE POTENTIELLE ELASTIQUE :
!        ==========================================================
            if ((option.eq.'ENEL_ELGA') .or. (option.eq.'ENEL_ELEM')) then
!
                do isig = 1, nbsm
                    eps(isig) = degpg((ipg-1)*8 + isig)
                    khi(isig) = degpg((ipg-1)*8 + isig +3)
                    if (isig .le. 2) then
                        gam(isig) = degpg((ipg-1)*8 + isig +6)
                    endif
                end do
!
! --- CALCUL DES PRODUITS :
!           MEMBRANE     : [DM]{EPSI}
!           FLEXION      : [DF]{KHI}
!           CISAILLEMENT : [DC]{GAM}
!
                eps(3) = eps(3)*2.d0
                khi(3) = khi(3)*2.d0
!
                call pmrvec('ZERO', 3, 3, dm, eps,&
                            dmeps)
                call pmrvec('ZERO', 3, 3, df, khi,&
                            dfkhi)
                call pmrvec('ZERO', 2, 2, dc, gam,&
                            dcgam)
!
                do isig = 1, nbsm
                    enelm(ipg) = enelm(ipg) + 0.5d0*eps(isig)*dmeps( isig)
                    enelf(ipg) = enelf(ipg) + 0.5d0*khi(isig)*dfkhi( isig)
                    if (isig .le. 2) then
                        enelc(ipg) = enelc(ipg) + 0.5d0*gam(isig)* dcgam(isig)
                    endif
                end do
!
! --- COUPLAGE MEMBRANE - FLEXION (ELAS_COQUE)
!
                if (coupmf) then
                    call pmrvec('ZERO', 3, 3, dmf, eps,&
                                dmeps)
                    call pmrvec('ZERO', 3, 3, dmf, khi,&
                                dfkhi)
!
                    do isig = 1, nbsm
                        enemf(ipg)= enemf(ipg)+0.5d0*(eps(isig)*dfkhi(isig)+ khi(isig)*dmeps(isig))
                    end do
                endif
!
                enelt(ipg) = enelm(ipg) + enelf(ipg)+ enelc(ipg) + enemf(ipg)
                enm = enm + enelm(ipg)*poids
                enf = enf + enelf(ipg)*poids
                enc = enc + enelc(ipg)*poids
                enmf = enmf + enemf(ipg)*poids
                ent = ent + enelt(ipg)*poids
            endif
        end do
    endif
!
! ---- RECUPERATION DU CHAMP DES DENSITES D'ENERGIE DE DEFORMATION
! ---- ELASTIQUE EN SORTIE
!      -------------------
    if (option .eq. 'ENEL_ELGA') then
        call jevech('PENERDR', 'E', idener)
    else if (option.eq.'ENEL_ELEM') then
        call jevech('PENERD1', 'E', idener)
    endif
!
! --- OPTION ENEL_ELGA
!     ================
    if (option .eq. 'ENEL_ELGA') then
        do ipg = 1, npg
            zr(idener-1+(ipg-1)*5 +1) = enelt(ipg)
            zr(idener-1+(ipg-1)*5 +2) = enelm(ipg)
            zr(idener-1+(ipg-1)*5 +3) = enelf(ipg)
            zr(idener-1+(ipg-1)*5 +4) = enelc(ipg)
            zr(idener-1+(ipg-1)*5 +5) = enemf(ipg)
        end do
!
! --- OPTION ENEL_ELEM
!     ================
    else if (option.eq.'ENEL_ELEM') then
        zr(idener ) = ent
        zr(idener +1) = enm
        zr(idener +2) = enf
        zr(idener +3) = enc
        zr(idener +4) = enmf
    else
!
!  --- OPTION NON DISPONIBLE
!      =====================
        valk(1) = option
        valk(2) = nomte
        valk(3) = zk16(icompo)
        call utmess('F', 'ELEMENTS_88', nk=3, valk=valk)
    endif
!
end subroutine
