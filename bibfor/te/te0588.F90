subroutine te0588(option, nomte)
    implicit none
#include "asterf_types.h"
#include "asterfort/epsthm.h"
#include "asterfort/eulnau.h"
#include "asterc/r8dgrd.h"
#include "asterfort/elref1.h"
#include "asterfort/iselli.h"
#include "asterc/ismaem.h"
#include "asterfort/jevech.h"
#include "asterfort/naueul.h"
#include "asterfort/posthm.h"
#include "asterfort/rcangm.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/refthm.h"
#include "asterfort/teattr.h"
#include "asterfort/tecach.h"
#include "asterfort/vecini.h"
#include "asterfort/xasshm.h"
#include "asterfort/xcaehm.h"
#include "asterfort/xfnohm.h"
#include "asterfort/xhmddl.h"
#include "asterfort/xhmini.h"
#include "asterfort/xpeshm.h"
#include "jeveux.h"
    character(len=16) :: option, nomte
!     ------------------------------------------------------------------
! =====================================================================
! person_in_charge: daniele.colombo at ifpen.fr
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
    integer :: ipoid2, ivf2, dimmat
    integer :: idfde2, npi, npg, nvim
!
    integer :: retloi, iret, iretp, iretm
    integer :: ipoids, ivf, idfde, igeom, idefo
    integer :: iinstp, ideplm, ideplp, idepla, icompo, icarcr, ipesa
    integer :: icontm, ivarip, ivarim, ivectu, icontp
! =====================================================================
! =====================================================================
    integer :: mecani(5), press1(7), press2(7), tempe(5), dimuel
    integer :: dimdef, dimcon, nbvari, nddls, nddlm
    integer :: nmec, np1, np2, i, ncmp, nnos, ichg, ichn
    integer :: jtab(7), igau, isig, nnom
    real(kind=8) :: defgep(13), defgem(13)
    real(kind=8) :: dfdi(20, 3), dfdi2(20, 3), b(17, 20*8)
    real(kind=8) :: drds(17, 11+5), drdsr(17, 11+5), dsde(11+5, 17)
    real(kind=8) :: r(17), sigbar(17), c(17), ck(17), cs(17)
    real(kind=8) :: epsm(405)
    real(kind=8) :: angmas(7), coor(3), angnau(3), angleu(3)
    character(len=3) :: modint
    character(len=8) :: typmod(2)
    character(len=16) :: phenom, elref
! =====================================================================
    integer :: li, ibid, yaenrm, idim
    real(kind=8) :: rho(1), rbid(100)
    integer :: icodre(1)
    aster_logical :: axi, perman
! =====================================================================
!  CETTE ROUTINE FAIT UN CALCUL EN HM AVEC XFEM
!  17 = (9 DEF MECA) + (3 DEF HEAV MECA) + 4 POUR P1 + 1 pour P1 HEAV
!  16 = 12 MECA + 4 POUR P1
! =====================================================================
!  POUR LES TABLEAUX DEFGEP ET DEFGEM ON A DANS L'ORDRE :
!                                      (PARTIE CLASSIQUE)
!                                      DX DY DZ
!                                      EPXX EPYY EPZZ EPXY EPXZ EPYZ
!                                      PRE1 P1DX P1DY P1DZ
!                                      (PARTIE ENRICHIE)
!                                      H1X  H1Y H1Z
!                                      HPRE1 
!            EPSXY = RAC2/2*(DU/DY+DV/DX)
! =====================================================================
!    POUR LES CHAMPS DE CONTRAINTE
!                                      SIXX SIYY SIZZ SIXY SIXZ SIYZ
!                                      SIPXX SIPYY SIPZZ SIPXY SIPXZ SIPYZ
!                                      M11 FH11X FH11Y FH11Z
!
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
! =====================================================================
    aster_logical :: fnoevo
    real(kind=8) :: dt
! =====================================================================
! DECLARATION POUR XFEM
!
    integer :: nfh
    integer :: ddld, ddlm, ddlp, nnop, nnops, nnopm
    integer :: enrmec(3), nenr, dimenr, enrhyd(3)
    integer :: jpintt, jcnset, jheavt, jpmilt
    integer :: jlonch, jbaslo, jlsn, jlst, jstno
    character(len=8) :: enr
! =====================================================================
! --- 1. INITIALISATIONS ----------------------------------------------
! --- SUIVANT ELEMENT, DEFINITION DES CARACTERISTIQUES : --------------
! --- CHOIX DU TYPE D'INTEGRATION -------------------------------------
! --- RECUPERATION DE LA GEOMETRIE ET POIDS DES POINTS D'INTEGRATION --
! --- RECUPERATION DES FONCTIONS DE FORME -----------------------------
! =====================================================================
! INITIALISATION POUR XFEM
!
    call xhmini(nomte, nfh, ddld, ddlm, ddlp)
    call xcaehm(nomte, axi, perman, typmod, modint,&
                mecani, press1, press2, tempe, dimdef,&
                dimcon, nmec, np1, np2, ndim,&
                nno, nnos, nnom, npi, npg,&
                nddls, nddlm, dimuel, ipoids, ivf,&
                idfde, ddld, ddlm, ddlp, enrmec, nenr,&
                dimenr, nnop, nnops, nnopm, enrhyd)
! =====================================================================
! --- PARAMETRES PROPRES A XFEM ---------------------------------------
! =====================================================================
    call jevech('PPINTTO', 'L', jpintt)
    call jevech('PCNSETO', 'L', jcnset)
    call jevech('PHEAVTO', 'L', jheavt)
    call jevech('PLONCHA', 'L', jlonch)
    call jevech('PBASLOR', 'L', jbaslo)
    call jevech('PLSN', 'L', jlsn)
    call jevech('PLST', 'L', jlst)
    call jevech('PSTANO', 'L', jstno)
    call elref1(elref)
!
! PARAMÈTRES PROPRES AUX ÉLÉMENTS 1D ET 2D QUADRATIQUES
!
    call teattr('S', 'XFEM', enr, ibid)
    if ((ibid.eq.0) .and. (enr.eq.'XH') .and. .not. iselli(elref)) call jevech('PPMILTO', 'L',&
                                                                               jpmilt)
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
!        call jevech('PVARIMP', 'L',
        read (zk16(icompo-1+2),'(I16)') nbvari
! =====================================================================
! ----RECUPERATION DES ANGLES NAUTIQUES/EULER DEFINIS PAR AFFE_CARA_ELEM
! --- ORIENTATION DU MASSIF
! --- COORDONNEES DU BARYCENTRE ( POUR LE REPRE CYLINDRIQUE )
! --- CONVERSION DES ANGLES NAUTIQUES EN ANGLES D'EULER
! =====================================================================
        call vecini(7, 0.d0, angmas)
        call vecini(3, 0.d0, coor)
        call vecini(3, 0.d0, angleu)
        call vecini(3, 0.d0, angnau)
!
        do 150 i = 1, nno
            do 140 idim = 1, ndim
                coor(idim) = coor(idim)+zr(igeom+idim+ndim*(i-1)-1)/ nno
140         continue
150     continue
        call rcangm(ndim, coor, angmas)
!# ANGMAS : donne par affe_cara_elem en degre et ici en fourni en radian
!# CAS OU AFFE_CARA_ELEM EST EN ANGLE D EULER => On CONVERTIT EN NAUTIQUE
        if (abs(angmas(4)-2.d0) .lt. 1.d-3) then
            if (ndim .eq. 3) then
                angleu(1) = angmas(5)
                angleu(2) = angmas(6)
                angleu(3) = angmas(7)
            else
                angleu(1) = angmas(5)
            endif
            call eulnau(angleu/r8dgrd(), angnau/r8dgrd())
!
!# CAS OU AFFE_CARA_ELEM EST EN ANGLE NAUTIQUE (OK PAS DE CONVERSION)
        else
            if (ndim .eq. 3) then
                angnau(1) = angmas(1)
                angnau(2) = angmas(2)
                angnau(3) = angmas(3)
            else
                angnau(1) = angmas(1)
            endif
        endif
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
        dimmat = nddls*nnop
        if (option(1:9) .eq. 'RIGI_MECA') then
            call xasshm(nno, npg, npi, ipoids, ivf,&
                        idfde, igeom, zr(igeom), zr(icarcr), zr(ideplm),&
                        zr(ideplm), zr(icontm), zr(icontm), zr(ivarim), zr(ivarim),&
                        defgem, defgep, drds, drdsr, dsde,&
                        b, dfdi, dfdi2, r, sigbar,&
                        c, ck, cs, zr(imatuu), zr( ivectu),&
                        zr(iinstm), zr(iinstp), option, zi(imate), mecani,&
                        press1, press2, tempe, dimdef, dimcon,&
                        dimuel, nbvari, nddls, nddlm, nmec,&
                        np1, ndim, zk16(icompo), axi, modint,&
                        retloi, nnop, nnops, nnopm, enrmec,&
                        dimenr, zi(jheavt), zi( jlonch), zi(jcnset), jpintt,&
                        jpmilt, jlsn, angnau,dimmat, enrhyd)
        else
            do 30 li = 1, dimuel
                zr(ideplp+li-1) = zr(ideplm+li-1) + zr(ideplp+li-1)
 30         continue
            call xasshm(nno, npg, npi, ipoids, ivf,&
                        idfde, igeom, zr(igeom), zr(icarcr), zr(ideplm),&
                        zr(ideplp), zr(icontm), zr(icontp), zr(ivarim), zr(ivarip),&
                        defgem, defgep, drds, drdsr, dsde,&
                        b, dfdi, dfdi2, r, sigbar,&
                        c, ck, cs, zr(imatuu), zr( ivectu),&
                        zr(iinstm), zr(iinstp), option, zi(imate), mecani,&
                        press1, press2, tempe, dimdef, dimcon,&
                        dimuel, nbvari, nddls, nddlm, nmec,&
                        np1, ndim, zk16(icompo), axi, modint,&
                        retloi, nnop, nnops, nnopm, enrmec,&
                        dimenr, zi(jheavt), zi( jlonch), zi(jcnset), jpintt,&
                        jpmilt, jlsn, angnau,dimmat, enrhyd)
            zi(jcret) = retloi
        endif
! =====================================================================
! --- SUPRESSION DES DDLS HEAVISIDE SUPERFLUS -------------------------
! =====================================================================
        call xhmddl(ndim, nddls, dimuel, nnop, nnops,&
                    zi(jstno), .false._1, option, nomte, zr(imatuu),&
                    zr(ivectu), nddlm)
    endif
! =====================================================================
! --- 3. OPTION : CHAR_MECA_PESA_R ------------------------------------
! =====================================================================
    if (option .eq. 'CHAR_MECA_PESA_R') then
        call jevech('PGEOMER', 'L', igeom)
        call jevech('PMATERC', 'L', imate)
        call jevech('PPESANR', 'L', ipesa)
        call jevech('PVECTUR', 'E', ivectu)
        call rccoma(zi(imate), 'THM_DIFFU', 1, phenom, icodre(1))
        call rcvalb('FPG1', 1, 1, '+', zi(imate),&
                    ' ', phenom, 0, ' ', [0.d0],&
                    1, 'RHO', rho(1), icodre, 1)
!
!        INDICATEUR POUR SAVOIR SI ON A DE L'ENRICHISSEMENT
        yaenrm = enrmec(1)
!
        call xpeshm(nno, nnop, nnops, ndim, nddls,&
                    nddlm, npg, igeom, jpintt, jpmilt, jlsn,&
                    ivf, ipoids, idfde, ivectu, ipesa,&
                    zi(jheavt), zi( jlonch), zi(jcnset), rho(1), axi,&
                    yaenrm)
!
! =====================================================================
! --- SUPRESSION DES DDLS HEAVISIDE SUPERFLUS -------------------------
! =====================================================================
        call xhmddl(ndim, nddls, dimuel, nnop, nnops,&
                    zi(jstno), .false._1, option, nomte, rbid,&
                    zr(ivectu), nddlm)
    endif
! ======================================================================
! --- 4. OPTION : FORC_NODA --------------------------------------------
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
        call tecach('ONN', 'PINSTMR', 'L', iretm, iad=iinstm)
        call tecach('ONN', 'PINSTPR', 'L', iretp, iad=iinstp)
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
!
        call xfnohm(fnoevo, dt, nno, npg, ipoids,&
                    ivf, idfde, zr(igeom), zr(icontm), b,&
                    dfdi, dfdi2, r, zr(ivectu), zi(imate),&
                    mecani, press1, dimcon, nddls, nddlm,&
                    dimuel, nmec, np1, ndim, axi,&
                    dimenr, nnop, nnops, nnopm, igeom,&
                    jpintt, jpmilt, jlsn, zi(jlonch), zi( jcnset), zi(jheavt),&
                    enrmec, enrhyd)
!
! =====================================================================
! --- SUPRESSION DES DDLS HEAVISIDE SUPERFLUS -------------------------
! =====================================================================
        call xhmddl(ndim, nddls, dimuel, nnop, nnops,&
                    zi(jstno), .false._1, option, nomte, rbid,&
                    zr(ivectu), nddlm)
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
        call tecach('OON', 'PVARIGR', 'L', iret, nval=7,&
                    itab=jtab)
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
210         continue
200     continue
    endif
! ======================================================================
end subroutine
