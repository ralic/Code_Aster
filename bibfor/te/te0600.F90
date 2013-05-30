subroutine te0600(option, nomte)
    implicit none
    include 'jeveux.h'
    include 'asterc/ismaem.h'
    include 'asterfort/assthm.h'
    include 'asterfort/caethm.h'
    include 'asterfort/dfdm2d.h'
    include 'asterfort/dfdm3d.h'
    include 'asterfort/epsthm.h'
    include 'asterfort/fnothm.h'
    include 'asterfort/jevech.h'
    include 'asterfort/posthm.h'
    include 'asterfort/rccoma.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/refthm.h'
    include 'asterfort/tecach.h'
    include 'asterfort/thmevc.h'
    character(len=16) :: option, nomte
! =====================================================================
! =====================================================================
! person_in_charge: sylvie.granet at edf.fr
! =====================================================================
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! =====================================================================
!    - FONCTION REALISEE:  CALCUL DES OPTIONS NON-LINEAIRES MECANIQUES
!                          ELEMENTS THHM, HM ET HH
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! =====================================================================
    integer :: jgano, nno, imatuu, ndim, imate, iinstm, jcret
    integer :: ipoid2, ivf2
    integer :: idfde2, npi, npg, nvim
!
    integer :: retloi, iret, iretp, iretm
    integer :: ipoids, ivf, idfde, igeom, idefo
    integer :: iinstp, ideplm, ideplp, idepla, icompo, icarcr, ipesa
    integer :: icontm, ivarip, ivarim, ivectu, icontp
! =====================================================================
! =====================================================================
    integer :: mecani(5), press1(7), press2(7), tempe(5), dimuel
    integer :: dimdep, dimdef, dimcon, nbvari, nddls, nddlm, ii
    integer :: nmec, np1, np2, i, ncmp, nnos, ichg, ichn
    integer :: jtab(7), igau, isig, nnom
    real(kind=8) :: defgep(21), defgem(21), dfdbid(27), poids
    real(kind=8) :: dfdi(20, 3), dfdi2(20, 3), b(21, 120), epsm(405)
    real(kind=8) :: drds(22, 31), drdsr(21, 31), dsde(31, 21)
    real(kind=8) :: r(22), sigbar(21), c(21), ck(21), cs(21)
    character(len=3) :: modint
    character(len=8) :: typmod(2)
    character(len=16) :: phenom
! =====================================================================
    integer :: li, kp, j, l, k, ibid, typvf
    real(kind=8) :: r8bid, rho, coef, rx
    integer :: icodre(1)
    logical :: axi, perman
! =====================================================================
!  CETTE ROUTINE FAIT UN CALCUL EN THHM , HM , HHM , THH
!  21 = 9 DEF MECA + 4 POUR P1 + 4 POUR P2 + 4 POUR T
!  31 = 7 MECA + 2*5 POUR P1 + 2*5 POUR P2 + 4 POUR T
! =====================================================================
!  POUR LES TABLEAUX DEFGEP ET DEFGEM ON A DANS L'ORDRE :
!                                      DX DY DZ
!                                      EPXX EPYY EPZZ EPXY EPXZ EPYZ
!                                      PRE1 P1DX P1DY P1DZ
!                                      PRE2 P2DX P2DY P2DZ
!                                      TEMP TEDX TEDY TEDZ
!            EPSXY = RAC2/2*(DU/DY+DV/DX)
! =====================================================================
!    POUR LES CHAMPS DE CONTRAINTE
!                                      SIXX SIYY SIZZ SIXY SIXZ SIYZ
!                                      SIP
!                                      M11 FH11X FH11Y FH11Z
!                                      ENT11
!                                      M12 FH12X FH12Y FH12Z
!                                      ENT12
!                                      M21 FH21X FH21Y FH21Z
!                                      ENT21
!                                      M22 FH22X FH22Y FH22Z
!                                      ENT22
!                                      QPRIM FHTX FHTY FHTZ
!        SIXY EST LE VRAI DE LA MECANIQUE DES MILIEUX CONTINUS
!        DANS EQUTHM ON LE MULITPLIERA PAR RAC2
! =====================================================================
!   POUR L'OPTION FORCNODA
!  SI LES TEMPS PLUS ET MOINS SONT PRESENTS
!  C'EST QUE L'ON APPELLE DEPUIS STAT NON LINE  : FNOEVO = VRAI
!  ET ALORS LES TERMES DEPENDANT DE DT SONT EVALUES
!  SI LES TEMPS PLUS ET MOINS NE SONT PAS PRESENTS
!  C'EST QUE L'ON APPELLE DEPUIS CALCNO  : FNOEVO = FAUX
!  ET ALORS LES TERMES DEPENDANT DE DT NE SONT PAS EVALUES
! =====================================================================
! AXI       AXISYMETRIQUE?
! TYPMOD    MODELISATION (D_PLAN, AXI, 3D ?)
! MODINT    METHODE D'INTEGRATION (CLASSIQUE,LUMPEE(D),REDUITE(R) ?)
! NNO       NB DE NOEUDS DE L'ELEMENT
! NNOS      NB DE NOEUDS SOMMETS DE L'ELEMENT
! NNOM      NB DE NOEUDS MILIEUX DE L'ELEMENT
! NDDLS     NB DE DDL SUR LES SOMMETS
! NDDLM     NB DE DDL SUR LES MILIEUX
! NPI       NB DE POINTS D'INTEGRATION DE L'ELEMENT
! NPG       NB DE POINTS DE GAUSS     POUR CLASSIQUE(=NPI)
!                 SOMMETS             POUR LUMPEE   (=NPI=NNOS)
!                 POINTS DE GAUSS     POUR REDUITE  (<NPI)
! NDIM      DIMENSION DE L'ESPACE
! DIMUEL    NB DE DDL TOTAL DE L'ELEMENT
! DIMCON    DIMENSION DES CONTRAINTES GENERALISEES ELEMENTAIRES
! DIMDEF    DIMENSION DES DEFORMATIONS GENERALISEES ELEMENTAIRES
! IVF       FONCTIONS DE FORMES QUADRATIQUES
! IVF2      FONCTIONS DE FORMES LINEAIRES
! =====================================================================
    logical :: fnoevo, vf
    real(kind=8) :: dt
! =====================================================================
! --- 1. INITIALISATIONS ----------------------------------------------
! --- SUIVANT ELEMENT, DEFINITION DES CARACTERISTIQUES : --------------
! --- CHOIX DU TYPE D'INTEGRATION -------------------------------------
! --- RECUPERATION DE LA GEOMETRIE ET POIDS DES POINTS D'INTEGRATION --
! --- RECUPERATION DES FONCTIONS DE FORME -----------------------------
! =====================================================================
    ibid = 0
    call caethm(nomte, axi, perman, vf, typvf,&
                typmod, modint, mecani, press1, press2,&
                tempe, dimdep, dimdef, dimcon, nmec,&
                np1, np2, ndim, nno, nnos,&
                nnom, ibid, npi, npg, nddls,&
                nddlm, ibid, ibid, dimuel, ipoids,&
                ivf, idfde, ipoid2, ivf2, idfde2,&
                ibid, jgano)
! =====================================================================
! --- DEBUT DES DIFFERENTES OPTIONS -----------------------------------
! =====================================================================
! --- 2. OPTIONS : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA -------------
! =====================================================================
    if ((option(1:9).eq.'RIGI_MECA' ) .or. (option(1:9).eq.'RAPH_MECA' ) .or.&
        (option(1:9).eq.'FULL_MECA' )) then
! =====================================================================
! --- PARAMETRES EN ENTREE --------------------------------------------
! =====================================================================
        call jevech('PGEOMER', 'L', igeom)
        call jevech('PMATERC', 'L', imate)
        call jevech('PINSTMR', 'L', iinstm)
        call jevech('PINSTPR', 'L', iinstp)
        call jevech('PDEPLMR', 'L', ideplm)
        call jevech('PDEPLPR', 'L', ideplp)
        call jevech('PCOMPOR', 'L', icompo)
        call jevech('PCARCRI', 'L', icarcr)
        call jevech('PVARIMR', 'L', ivarim)
        call jevech('PCONTMR', 'L', icontm)
        read (zk16(icompo-1+2),'(I16)') nbvari
! =====================================================================
! --- PARAMETRES EN SORTIE ISMAEM? ------------------------------------
! =====================================================================
        if (option(1:9) .eq. 'RIGI_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
            call jevech('PMATUNS', 'E', imatuu)
        else
            imatuu = ismaem()
        endif
        if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
            call jevech('PVECTUR', 'E', ivectu)
            call jevech('PCONTPR', 'E', icontp)
            call jevech('PVARIPR', 'E', ivarip)
            call jevech('PCODRET', 'E', jcret)
            zi(jcret) = 0
        else
            ivectu = ismaem()
            icontp = ismaem()
            ivarip = ismaem()
        endif
        retloi = 0
        if (option(1:9) .eq. 'RIGI_MECA') then
            call assthm(nno, nnos, nnom, npg, npi,&
                        ipoids, ipoid2, ivf, ivf2, idfde,&
                        idfde2, zr(igeom), zr(icarcr), zr(ideplm), zr(ideplm),&
                        zr(icontm), zr(icontm), zr(ivarim), zr(ivarim), defgem,&
                        defgep, drds, drdsr, dsde, b,&
                        dfdi, dfdi2, r, sigbar, c,&
                        ck, cs, zr(imatuu), zr(ivectu), zr(iinstm),&
                        zr(iinstp), option, zi( imate), mecani, press1,&
                        press2, tempe, dimdef, dimcon, dimuel,&
                        nbvari, nddls, nddlm, nmec, np1,&
                        np2, ndim, zk16(icompo), typmod, axi,&
                        perman, modint, retloi)
        else
            do 30 li = 1, dimuel
                zr(ideplp+li-1) = zr(ideplm+li-1) + zr(ideplp+li-1)
30          continue
            call assthm(nno, nnos, nnom, npg, npi,&
                        ipoids, ipoid2, ivf, ivf2, idfde,&
                        idfde2, zr(igeom), zr(icarcr), zr(ideplm), zr(ideplp),&
                        zr(icontm), zr(icontp), zr(ivarim), zr(ivarip), defgem,&
                        defgep, drds, drdsr, dsde, b,&
                        dfdi, dfdi2, r, sigbar, c,&
                        ck, cs, zr(imatuu), zr(ivectu), zr(iinstm),&
                        zr(iinstp), option, zi( imate), mecani, press1,&
                        press2, tempe, dimdef, dimcon, dimuel,&
                        nbvari, nddls, nddlm, nmec, np1,&
                        np2, ndim, zk16(icompo), typmod, axi,&
                        perman, modint, retloi)
            zi(jcret) = retloi
        endif
    endif
! =====================================================================
! --- 3. OPTION : CHAR_MECA_PESA_R ------------------------------------
! =====================================================================
    if (option .eq. 'CHAR_MECA_PESA_R') then
        call jevech('PGEOMER', 'L', igeom)
        call jevech('PMATERC', 'L', imate)
        call jevech('PPESANR', 'L', ipesa)
        call jevech('PVECTUR', 'E', ivectu)
        call rccoma(zi(imate), 'THM_DIFFU', 1, phenom, icodre)
        call rcvalb('FPG1', 1, 1, '+', zi(imate),&
                    ' ', phenom, 0, ' ', r8bid,&
                    1, 'RHO', rho, icodre, 1)
        if (ndim .eq. 3) then
! =====================================================================
! --- CAS 3D ----------------------------------------------------------
! =====================================================================
            do 40 i = 1, dimuel
                zr(ivectu+i-1) = 0.0d0
40          continue
! =====================================================================
! --- BOUCLE SUR LES POINTS DE GAUSS ----------------------------------
! =====================================================================
            do 70 kp = 1, npg
                l = (kp-1)*nno
                call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                            dfdbid, dfdbid, dfdbid, poids)
                coef = rho*poids*zr(ipesa)
                do 60 i = 1, nnos
                    ii = nddls* (i-1)
                    do 50 j = 1, 3
                        zr(ivectu+ii+j-1) = zr(ivectu+ii+j-1) + coef*zr(ivf+l+i-1)*zr(ipesa+j)
50                  continue
60              continue
                do 65 i = 1, nnom
                    ii = nnos*nddls+nddlm*(i-1)
                    do 55 j = 1, 3
                        zr(ivectu+ii+j-1) = zr(ivectu+ii+j-1) + coef*zr(ivf+l+i+nnos-1)*zr(ipesa+&
                                            &j)
55                  continue
65              continue
70          continue
        else
! =====================================================================
! --- CAS 2D ----------------------------------------------------------
! =====================================================================
            do 110 kp = 1, npg
                k = (kp-1)*nno
                call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                            dfdbid, dfdbid, poids)
                poids = poids*rho*zr(ipesa)
                if (axi) then
                    rx = 0.d0
                    do 80 i = 1, nno
                        rx = rx + zr(igeom+2*i-2)*zr(ivf+k+i-1)
80                  continue
                    poids = poids*rx
                    do 90 i = 1, nnos
                        zr(ivectu+nddls*(i-1)+1)=zr(ivectu+nddls*(i-1)&
                        +1) +poids*zr(ipesa+2)*zr(ivf+k+i-1)
90                  continue
                    do 95 i = 1, nnom
                        zr(ivectu+nddls*nnos+nddlm*(i-1)+1) = zr(&
                                                              ivectu+nddls*nnos+nddlm*(i-1)+1) + &
                                                              &poids*zr( ipesa+2)*zr(ivf+k+i+nnos&
                                                              &-1&
                                                              )
95                  continue
                else
!
                    do 100 i = 1, nnos
                        zr(ivectu+nddls*(i-1)) = zr(&
                                                 ivectu+nddls*(i-1) ) +poids*zr(ipesa+1)*zr(ivf+k&
                                                 &+i-1&
                                                 )
                        zr(ivectu+nddls*(i-1)+1)=zr(ivectu+nddls*(i-1)&
                        +1) +poids*zr(ipesa+2)*zr(ivf+k+i-1)
100                  continue
                    do 400 i = 1, nnom
                        zr(ivectu+nddls*nnos+nddlm*(i-1))= zr(ivectu+&
                        nddls*nnos+nddlm*(i-1)) + poids*zr(ipesa+1)*&
                        zr(ivf+k+i+nnos-1)
                        zr(ivectu+nddls*nnos+nddlm*(i-1)+1)= zr(&
                        ivectu+nddls*nnos+nddlm*(i-1)+1) + poids*zr(&
                        ipesa+2)*zr(ivf+k+i+nnos-1)
400                  continue
                endif
110          continue
        endif
    endif
!
! =====================================================================
! --- 4. OPTIONS : CHAR_MECA_FR2D2D OU CHAR_MECA_FR3D3D ---------------
! =====================================================================
!
    call thmevc(option, nomte, axi, nno, npg,&
                ipoids, ivf, idfde, nddls, nnos,&
                nddlm, nnom)
!
! ======================================================================
! --- 5. OPTION : FORC_NODA --------------------------------------------
! ======================================================================
    if (option .eq. 'FORC_NODA') then
! ======================================================================
! --- PARAMETRES EN ENTREE ---------------------------------------------
! ======================================================================
        call jevech('PGEOMER', 'L', igeom)
        call jevech('PCONTMR', 'L', icontm)
        call jevech('PMATERC', 'L', imate)
! ======================================================================
! --- SI LES TEMPS PLUS ET MOINS SONT PRESENTS -------------------------
! --- C EST QUE L ON APPELLE DEPUIS STAT NON LINE ET -------------------
! --- ALORS LES TERMES DEPENDANT DE DT SONT EVALUES --------------------
! ======================================================================
        call tecach('ONN', 'PINSTMR', 'L', 1, iinstm,&
                    iretm)
        call tecach('ONN', 'PINSTPR', 'L', 1, iinstp,&
                    iretp)
        if (iretm .eq. 0 .and. iretp .eq. 0) then
            dt = zr(iinstp) - zr(iinstm)
            fnoevo = .true.
        else
            fnoevo = .false.
            dt = 0.d0
        endif
! ======================================================================
! --- PARAMETRES EN SORTIE ---------------------------------------------
! ======================================================================
        call jevech('PVECTUR', 'E', ivectu)
        call fnothm(fnoevo, dt, perman, nno, nnos,&
                    nnom, npi, npg, ipoids, ipoid2,&
                    ivf, ivf2, idfde, idfde2, zr(igeom),&
                    zr(icontm), b, dfdi, dfdi2, r,&
                    zr(ivectu), zi(imate), mecani, press1, press2,&
                    tempe, dimdef, dimcon, nddls, nddlm,&
                    dimuel, nmec, np1, np2, ndim,&
                    axi)
    endif
! ======================================================================
! --- 6. OPTION : REFE_FORC_NODA ---------------------------------------
! ======================================================================
    if (option .eq. 'REFE_FORC_NODA') then
! ======================================================================
! --- ON RAPPELLE QUE LES PARAMETRES DU CRITERE DE CONVERGENCE SONT ----
! --- STOCKES DE LA FACON SUIVANTE : (1) : SIGM_REFE -------------------
! ---------------------------------- (3) : FLUX_THER_REFE --------------
! ---------------------------------- (4) : FLUX_HYD1_REFE --------------
! ---------------------------------- (5) : FLUX_HYD2_REFE --------------
! ======================================================================
! --- INITIALISATION ---------------------------------------------------
! ======================================================================
        dt = 1.0d0
        fnoevo = .true.
! ======================================================================
! --- PARAMETRES EN ENTREE ---------------------------------------------
! ======================================================================
        call jevech('PGEOMER', 'L', igeom)
        call jevech('PMATERC', 'L', imate)
! ======================================================================
! --- PARAMETRES EN SORTIE ---------------------------------------------
! ======================================================================
        call jevech('PVECTUR', 'E', ivectu)
! ======================================================================
! --- APPEL A LA ROUTINE SUR LES CRITERES DE CONVERGENCE ---------------
! ======================================================================
        call refthm(fnoevo, dt, perman, nno, nnos,&
                    nnom, npi, npg, ipoids, ipoid2,&
                    ivf, ivf2, idfde, idfde2, zr(igeom),&
                    b, dfdi, dfdi2, r, zr( ivectu),&
                    zi(imate), mecani, press1, press2, tempe,&
                    dimdef, dimcon, dimuel, nddls, nddlm,&
                    nmec, np1, np2, ndim, axi)
    endif
! ======================================================================
! --- 7. OPTION : SIEF_ELNO --------------------------------------------
! ======================================================================
    if (option .eq. 'SIEF_ELNO  ') then
        ncmp = dimcon
        call jevech('PCONTRR', 'L', ichg)
        call jevech('PSIEFNOR', 'E', ichn)
        nvim = mecani(5)
        call posthm(option, modint, jgano, ncmp, nvim,&
                    zr(ichg), zr(ichn))
    endif
! ======================================================================
! --- 8. OPTION : VARI_ELNO --------------------------------------------
! ======================================================================
    if (option .eq. 'VARI_ELNO  ') then
        call jevech('PVARIGR', 'L', ichg)
        call jevech('PVARINR', 'E', ichn)
!
        call jevech('PCOMPOR', 'L', icompo)
        read (zk16(icompo+1),'(I16)') ncmp
        read (zk16(icompo-1+7+9+4),'(I16)') nvim
        call tecach('OON', 'PVARIGR', 'L', 7, jtab,&
                    iret)
!
        call posthm(option, modint, jgano, ncmp, nvim,&
                    zr(ichg), zr(ichn))
    endif
! ======================================================================
! --- 9. OPTION : EPSI_ELGA --------------------------------------------
! ======================================================================
    if (option .eq. 'EPSI_ELGA') then
        call jevech('PGEOMER', 'L', igeom)
        call jevech('PDEPLAR', 'L', idepla)
        call jevech('PDEFOPG', 'E', idefo)
        call epsthm(nddls, nddlm, nno, nnos, nnom,&
                    nmec, dimdef, dimuel, ndim, npi,&
                    ipoids, ipoid2, ivf, ivf2, idfde,&
                    idfde2, dfdi, dfdi2, b, zr(igeom),&
                    zr(idepla), mecani, press1, press2, tempe,&
                    np1, np2, axi, epsm)
!
        do 200 igau = 1, npi
            do 210 isig = 1, 6
                zr(idefo+6*(igau-1)+isig-1) = epsm(6*(igau-1)+isig)
210          continue
200      continue
    endif
! ======================================================================
end subroutine
