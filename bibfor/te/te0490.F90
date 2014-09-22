subroutine te0490(option, nomte)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! aslint: disable=,W0104
    implicit none
!
!     BUTS: .CALCUL DES INDICATEURS GLOBAUX
!            DE PERTE DE PROPORTIONNALITE DU CHARGEMENT
!            POUR LES ELEMENTS ISOPARAMETRIQUES 2D
!           .CALCUL DES ENERGIES DE DEFORMATION ELASTIQUE ET TOTALE
!
! -----------------------------------------------------------------
!
!  OPTION INDIC_ENER : CALCUL DE  L'INDICATEUR GLOBAL
!  =================   ENERGETIQUE DETERMINE PAR L'EXPRESSION SUIVANTE:
!
!            IE = (SOMME_DOMAINE((1 - PSI(EPS)/OMEGA(EPS,VARI)).DV)/V
!
!        OU  .OMEGA EST LA DENSITE D'ENERGIE TOTALE
!            (I.E. OMEGA = SOMME_0->T(SIGMA:D(EPS)/DT).DTAU
!            .PSI EST LA DENSITE D'ENERGIE ELASTIQUE 'TOTALE'
!            (I.E. ASSOCIEE A LA COURBE DE TRACTION SI ON
!                  CONSIDERAIT LE MATERIAU ELASTIQUE NON-LINEAIRE)
!            .V EST LE VOLUME DU GROUPE DE MAILLES TRAITE
! -----------------------------------------------------------------
!
!  OPTION INDIC_SEUIL : CALCUL DE  L'INDICATEUR GLOBAL
!  ==================   DETERMINE PAR L'EXPRESSION SUIVANTE :
!
!   IS = (SOMME_DOMAINE(1 - ((SIG-X):EPS_PLAST)/((SIG_Y+R)*P)).DV)/V
!
!        OU  .SIG       EST LE TENSEUR DES CONTRAINTES
!            .X         EST LE TENSEUR DE RAPPEL
!            .EPS_PLAST EST LE TENSEUR DES DEFORMATIONS PLASTIQUES
!            .SIG_Y     EST LA LIMITE D'ELASTICITE
!            .R         EST LA FONCTION D'ECROUISSAGE
!            .P         EST LA DEFORMATION PLASTIQUE CUMULEE
!            .V         EST LE VOLUME DU GROUPE DE MAILLES TRAITE
!
! -----------------------------------------------------------------
!
! -----------------------------------------------------------------
!
!  OPTION ENEL_ELEM : CALCUL DE L'ENERGIE DE DEFORMATION ELASTIQUE
!  ================   DETERMINEE PAR L'EXPRESSION SUIVANTE :
!
!  EN HPP
!   ENELAS =  SOMME_VOLUME((SIG_T*(1/D)*SIG).DV)
!
!        OU  .SIG       EST LE TENSEUR DES CONTRAINTES DE CAUCHY
!            .D         EST LE TENSEUR DE HOOKE
!
!  EN GRANDES DEFORMATIONS SIMO MIEHE POUR ELAS OU VMIS_ISOT
!   ENERLAS = ENERGIE ELASTIQUE SPECIFIQUE
!           = K(0.5(J^2-1)-lnJ)+0.5mu(tr(J^(-2/3)be)-3)
!           SI PRESENCE DE THERMIQUE, ON AJOUTE UNE CORRECTION
!           SPECIFIQUE PRESENTEE DANS LA DOC R
!  EN GRANDES DEFORMATIONS GDEF_LOG
!   ENERELAS = SOMME_VOLUME((T_T*(1/D)*T).DV)
!        OU  .T       EST LE TENSEUR DES CONTRAINTES DU FORMALISME
!            .D         EST LE TENSEUR DE HOOKE
! -----------------------------------------------------------------
!
!  OPTION ENER_TOTALE : CALCUL DE L'ENERGIE DE DEFORMATION TOTALE
!  ==================   DETERMINEE PAR LES EXPRESSIONS SUIVANTES :
!
!   1-   ENER_TOTALE =  ENELAS + EPLAS
!
!          AVEC : ENELAS =  SOMME_VOLUME((SIG_T*(1/D)*SIG).DV)
!                 ENELAS EST L'ENERGIE DE DEFORMATION ELASTIQUE
!
!           OU  .SIG       EST LE TENSEUR DES CONTRAINTES
!               .D         EST LE TENSEUR DE HOOKE
!
!          ET   : EPLAS = SOMME_VOLUME((R(P))*D(P))
!                 EPLAS EST L'ENERGIE DE DEFORMATION PLASTIQUE
!
!           OU  .P         EST LA DEFORMATION PLASTIQUE CUMULEE
!           ET   R(P) EST CALCULE POUR LES COMPORTEMENTS SUIVANTS :
!                      .VMIS_ISOT_LINE
!                      .VMIS_ISOT_TRAC
!
!   2-   ENER_TOTALE= SOMME_VOLUME(OMEGA_ELEMENTAIRE)
!
!           AVEC : OMEGA_ELEMENTAIRE=
!            1/2(SIGMA(T1)*EPSI(T1)+SOMME(SIGMA(T(I))*DELTA(EPSI))+
!                SOMME(SIGMA(T(I+1))*DELTA(EPSI))
!
!   REMARQUE : EN GRANDE DEFORMATION ON INTEGRE SUR LE VOLUME INITIALE
! -----------------------------------------------------------------
!          ELEMENTS ISOPARAMETRIQUES 2D
!
!          OPTIONS : 'INDIC_ENER'
!                    'INDIC_SEUIL'
!                    'ENEL_ELEM'
!                    'ENER_TOTALE'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/enelpg.h"
#include "asterfort/eps1mc.h"
#include "asterfort/epsvmc.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/nbsigm.h"
#include "asterfort/nmgeom.h"
#include "asterfort/ortrep.h"
#include "asterfort/rcfonc.h"
#include "asterfort/rctrac.h"
#include "asterfort/rctype.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "asterfort/verift.h"
!-----------------------------------------------------------------------
    integer :: idconm, idene1, idene2, idepl, ideplm, idepmm
    integer :: idfde, idsig, idsigm, idvari, igau, igeom, imate, itemps
    integer :: ipoids, iret1, ivf, jgano, jprol, jvale, mxcmel
    integer :: nbsgm, nbsig, nbsig2, nbval, nbvari, ndim, nno
    integer :: nnos, npg, iret, idim, i, jtab(7), icodre(5)
    parameter (mxcmel = 162)
    parameter (nbsgm  =   6)
    real(kind=8) :: airep, c1, c2, deux, deuxmu, dsde, e
    real(kind=8) :: enelas, eneldv, enelsp, enelto, eplaeq, eplast, epseq
    real(kind=8) :: epsthe, omega, p, poids, psi, rp
    real(kind=8) :: rprim, sigeq, sigy, tempg, trepsm, trois, trsig
    real(kind=8) :: un, undemi, untier, volume, welas, wtotal
    real(kind=8) :: zero, valres(5)
    real(kind=8) :: sigma(nbsgm), epsdv(nbsgm)
    real(kind=8) :: epsel(nbsgm), epspla(nbsgm), x(nbsgm)
    real(kind=8) :: epsim(nbsgm), delta(nbsgm), sigmm(nbsgm)
    real(kind=8) :: epsi(nbsgm), epssm(mxcmel), epss(mxcmel)
    real(kind=8) :: repere(7), instan, nharm, integ, integ1
    real(kind=8) :: epsm(mxcmel), integ2, nu, k, indigl, xyz(3), para_vale
    real(kind=8) :: f(3, 3), r, eps(6), trav(81)
    character(len=4) :: fami
    character(len=8) :: para_type
    character(len=16) :: nomres(5), nomte, option, optio2, compor(3)
    aster_logical :: grand, axi
!-----------------------------------------------------------------------
!
!
! ---- INITIALISATIONS :
!
    zero = 0.0d0
    undemi = 0.5d0
    un = 1.0d0
    deux = 2.0d0
    trois = 3.0d0
    untier = 1.0d0/3.0d0
    nharm = zero
    omega = zero
    psi = zero
    volume = zero
    indigl = zero
    enelas = zero
    eplast = zero
    welas = zero
    wtotal = zero
    instan = zero
!
! ---- CARACTERISTIQUES DU TYPE D'ELEMENT :
! ---- GEOMETRIE ET INTEGRATION
!
!
    fami = 'RIGI'
    call elrefe_info(fami=fami, ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jvf=ivf, jdfde=idfde, jgano=jgano)
!
! --- TYPE DE MODELISATION
!
    if (lteatt('AXIS','OUI')) then
        axi=.true.
    else
        axi=.false.
    endif
!
! ---- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT :
!
    nbsig = nbsigm()
!
! ---- RECUPERATION DES COORDONNEES DES CONNECTIVITES :
!
    call jevech('PGEOMER', 'L', igeom)
!
! ---- RECUPERATION DU MATERIAU :
!
    call jevech('PMATERC', 'L', imate)
!
! ---- RECUPERATION  DES DONNEEES RELATIVES AU REPERE D'ORTHOTROPIE :
!     COORDONNEES DU BARYCENTRE ( POUR LE REPRE CYLINDRIQUE )
!
    xyz(1) = 0.d0
    xyz(2) = 0.d0
    xyz(3) = 0.d0
    do i = 1, nno
        do idim = 1, ndim
            xyz(idim) = xyz(idim)+zr(igeom+idim+ndim*(i-1)-1)/nno
        end do
    end do
!
    call ortrep(zi(imate), ndim, xyz, repere)
!
! ---- RECUPERATION DU CHAMP DE DEPLACEMENTS AUX NOEUDS  :
!
    call jevech('PDEPLR', 'L', idepl)
!
! ---- RECUPERATION DU CHAMP DE CONTRAINTES AUX POINTS D'INTEGRATION :
!
    call jevech('PCONTPR', 'L', idsig)
!
! ---- RECUPERATION DE L'INSTANT DE CALCUL
!      -----------------------------------
    call tecach('NNN', 'PTEMPSR', 'L', iret, iad=itemps)
    if (itemps .ne. 0) instan = zr(itemps)
!
! ----RECUPERATION DU TYPE DE COMPORTEMENT  :
!     N'EXISTE PAS EN LINEAIRE
    call tecach('NNN', 'PCOMPOR', 'L', iret, nval=7,&
                itab=jtab)
    compor(1)='ELAS'
    compor(2)=' '
    compor(3)='PETIT'
    if (iret .eq. 0) then
        compor(1)=zk16(jtab(1))
        compor(3)=zk16(jtab(1)+2)
    endif
!
!     GRANDES DEFORMATIONS
!
    if ((compor(3).eq.'SIMO_MIEHE') .or. (compor(3).eq.'GDEF_LOG') .or.&
        (compor(3).eq.'GDEF_HYPO_ELAS')) then
        grand = .true.
    else
        grand = .false.
    endif
!
! ON TESTE LA RECUPERATION DU CHAMP DE CONTRAINTES DU PAS PRECEDENT
!
    if (option .eq. 'ENER_TOTALE') then
        if (grand) then
            call utmess('F', 'COMPOR1_78', sk=compor(3))
        endif
        if ((compor(1)(1:9).ne.'VMIS_ISOT') .and. (compor(1)(1:4) .ne.'ELAS')) then
            call tecach('NNN', 'PCONTMR', 'L', iret, iad=idconm)
            if (idconm .ne. 0) then
                call jevech('PCONTMR', 'L', idsigm)
            endif
        endif
    endif
!
!
! ---- RECUPERATION DU CHAMP DE DEPLACEMENTS AUX NOEUDS
! AU PAS PRECEDENT :
!
    if (option .eq. 'ENER_TOTALE') then
        call tecach('NNN', 'PDEPLM', 'L', iret, iad=ideplm)
        if (ideplm .ne. 0) then
            call jevech('PDEPLM', 'L', idepmm)
        endif
    endif
!
! ----   RECUPERATION DU CHAMP DE VARIABLES INTERNES  :
!        N'EXISTE PAS EN LINEAIRE
    call tecach('ONN', 'PVARIPR', 'L', iret, nval=7,&
                itab=jtab)
    if (iret .eq. 0) then
        idvari=jtab(1)
        nbvari = max(jtab(6),1)*jtab(7)
    else
        idvari=1
        nbvari=0
    endif
!
! -- CALCUL DES DEFORMATIONS TOTALES DANS LE CAS DE
! -- ENERGIE TOTALE A L INSTANT COURANT ET CELUI D AVANT
!
    if ((compor(1)(1:9).ne.'VMIS_ISOT') .and. (compor(1)(1:4).ne.'ELAS')) then
!
        if (option .eq. 'ENER_TOTALE') then
!
!  CALCUL DE B.U
!
            call eps1mc(nno, ndim, nbsig, npg, ipoids,&
                        ivf, idfde, zr(igeom), zr(idepl), nharm,&
                        epss)
!
            if (ideplm .ne. 0) then
                call eps1mc(nno, ndim, nbsig, npg, ipoids,&
                            ivf, idfde, zr(igeom), zr(idepmm), nharm,&
                            epssm)
            endif
        endif
    endif
!
! ---- CALCUL DES DEFORMATIONS HORS THERMIQUES CORRESPONDANTES AU
! ---- CHAMP DE DEPLACEMENT I.E. EPSM = EPST - EPSTH
! ---- OU EPST  SONT LES DEFORMATIONS TOTALES
! ----    EPST = B.U
! ---- ET EPSTH SONT LES DEFORMATIONS THERMIQUES
! ----    EPSTH = ALPHA*(T-TREF) :
!         ----------------------
    optio2 = 'EPME_ELGA'
    call epsvmc(fami, nno, ndim, nbsig, npg,&
                ipoids, ivf, idfde, zr(igeom), zr(idepl),&
                instan, zi(imate), repere, nharm, optio2,&
                epsm)
!
!                      ===========================
!                      =                         =
!                      = OPTION   "INDIC_ENER"   =
!                      = OPTION   "ENEL_ELEM"    =
!                      = OPTION   "ENER_TOTALE"  =
!                      =                         =
!                      ===========================
!
    if (option .eq. 'INDIC_ENER' .or. option .eq. 'ENEL_ELEM' .or. option .eq.&
        'ENER_TOTALE') then
!
! --- BOUCLE SUR LES POINTS D'INTEGRATION
!
        do 10 igau = 1, npg
!
            omega = zero
            psi = zero
!
! --- TENSEUR DES CONTRAINTES AU POINT D'INTEGRATION COURANT :
!
            do 20 i = 1, nbsig
                sigma(i) = zr(idsig+(igau-1)*nbsig+i-1)
 20         continue
!
! --- CALCUL DU JACOBIEN AU POINT D'INTEGRATION COURANT :
!
            call nmgeom(2, nno, axi, grand, zr(igeom),&
                        igau, ipoids, ivf, idfde, zr(idepl),&
                        .true._1, poids, trav, f, eps,&
                        r)
!
            call enelpg(fami, zi(imate), instan, igau, repere,&
                        xyz, compor, f, sigma, nbvari,&
                        zr(idvari+(igau-1)*nbvari), enelas)
!
!
!
            if (option .eq. 'ENEL_ELEM') then
!
                welas = welas + enelas*poids
!
                goto 10
!
!  ===============================================
!  = FIN TRAITEMENT DE L'OPTION ENEL_ELEM        =
!  ===============================================
!
!
            endif
!
! --- RECUPERATION DES CARACTERISTIQUES DU MATERIAU :
!
            nomres(1) = 'E'
            nomres(2) = 'NU'
!
            call rcvalb(fami, igau, 1, '+', zi(imate),&
                        ' ', 'ELAS', 0, ' ', [0.d0],&
                        2, nomres, valres, icodre, 2)
!
!
            e = valres(1)
            nu = valres(2)
!
!-------------------------------------------------------
!   CACUL DU TERME OMEGA REPRESENTANT L'ENERGIE TOTALE -
!   OMEGA = SOMME_0->T(SIGMA:D(EPS)/DT).DTAU           -
!-------------------------------------------------------
! --- TRAITEMENT DU CAS DE L'ECROUISSAGE LINEAIRE ISOTROPE :
!
            if (compor(1) .eq. 'VMIS_ISOT_LINE') then
!
! --- RECUPERATION DE LA LIMITE D'ELASTICITE SY
! --- ET DE LA PENTE DE LA COURBE DE TRACTION D_SIGM_EPSI :
!
                nomres(1) = 'D_SIGM_EPSI'
                nomres(2) = 'SY'
!
                call rcvalb(fami, igau, 1, '+', zi(imate),&
                            ' ', 'ECRO_LINE', 0, ' ', [0.d0],&
                            2, nomres, valres, icodre, 2)
!
                dsde = valres(1)
                sigy = valres(2)
!
! --- RECUPERATION DE LA DEFORMATION PLASTIQUE CUMULEE :
!
                p = zr(idvari+(igau-1)*nbvari+1-1)
!
! --- PENTE DE LA COURBE DE TRACTION DANS LE DIAGRAMME 'REDRESSE' :
!
                rprim = e*dsde/(e-dsde)
!
! --- CONTRAINTE UNIAXIALE SUR LA COURBE DE TRACTION :
!
                rp = sigy + rprim*p
!
! --- TRAVAIL PLASTIQUE 'EQUIVALENT' :
!
                eplast = undemi*(sigy+rp)*p
!
! --- TRAITEMENT DU CAS DE L'ECROUISSAGE NON-LINEAIRE ISOTROPE :
!
            else if (compor(1).eq.'VMIS_ISOT_TRAC') then
!
! --- RECUPERATION DE LA COURBE DE TRACTION :
!
!  --- TEMPERATURE AU POINT D'INTEGRATION COURANT :
!
                call rcvarc(' ', 'TEMP', '+', fami, igau,&
                            1, tempg, iret1)
                call rctype(zi(imate), 1, 'TEMP', [tempg], para_vale,&
                            para_type)
                if ((para_type(1:4).eq.'TEMP') .and. (iret1.eq.1)) then
                    call utmess('F', 'COMPOR5_5', sk = para_type)
                endif
                call rctrac(zi(imate), 1, 'SIGM', para_vale, jprol,&
                            jvale, nbval, e)
!
! --- RECUPERATION DE LA DEFORMATION PLASTIQUE CUMULEE :
!
                p = zr(idvari+(igau-1)*nbvari+1-1)
!
! --- TRAVAIL PLASTIQUE 'EQUIVALENT' :
!
                call rcfonc('V', 1, jprol, jvale, nbval,&
                            p = p, rp = rp, rprim = rprim, airerp = airep)
!
                eplast = airep
            endif
!
! ---  AFFECTATION A OMEGA QUI EST L'ENERGIE TOTALE
! ---  DE LA CONTRIBUTION AU POINT D'INTEGRATION DE L'ENERGIE
! ---  ELASTIQUE ET DE L'ENERGIE PLASTIQUE :
!
            omega = omega + enelas + eplast
!
! ---  TRAITEMENT DE L'OPTION ENER_TOTALE :
!
            if (option .eq. 'ENER_TOTALE') then
!
                if ((compor(1)(1:9) .ne.'VMIS_ISOT') .and. ( compor(1)( 1:4).ne.'ELAS')) then
!
! ---       TENSEUR DES CONTRAINTES AU POINT D'INTEGRATION PRECEDENT
!           ON LE CALCULE SEULEMENT DANS LE CAS DE LOI DE COMPORTEMENT
!           NI VMIS_ISOT_ NI ELAS
!
                    if (idconm .ne. 0) then
                        do 25 i = 1, nbsig
                            sigmm(i) = zr(idsigm+(igau-1)*nbsig+i-1)
 25                     continue
                    endif
!
! ---       TENSEUR DES DEFORMATIONS AU POINT D'INTEGRATION COURANT
!           ON LE CALCULE SEULEMENT DANS LE CAS DE LOI DE COMPORTEMENT
!           NI VMIS_ISOT_ NI ELAS
!
                    do 30 i = 1, nbsig
                        epsi(i)= epss(i+(igau-1)*nbsig)
 30                 continue
!
! ---      TENSEUR DES DEFORMATIONS AU POINT D'INTEGRATION PRECEDENT
!          ON LE CALCULE SEULEMENT DANS LE CAS DE LOI DE COMPORTEMENT
!          NI VMIS_ISOT_ NI ELAS
!
                    if (ideplm .ne. 0) then
                        do 35 i = 1, nbsig
                            epsim(i) = epssm(i+(igau-1)*nbsig)
 35                     continue
                    endif
!
                    if ((idconm.ne.0) .and. (ideplm.ne.0)) then
                        do 50 i = 1, nbsig
                            delta(i)=epsi(i)-epsim(i)
 50                     continue
!
!---          CALCUL DES TERMES A SOMMER
!
                        integ1=sigmm(1)*delta(1)+sigmm(2)*delta(2)+&
                        sigmm(3)*delta(3)+2.0d0*sigmm(4)*delta(4)
                        integ2=sigma(1)*delta(1)+sigma(2)*delta(2)+&
                        sigma(3)*delta(3)+2.0d0*sigma(4)*delta(4)
                        integ=undemi*(integ1+integ2)*poids
!
                    else
!
!---          CAS OU LE NUMERO D ORDRE EST UN
!
                        integ= sigma(1)*epsi(1)+sigma(2)*epsi(2)+&
                        sigma(3)*epsi(3)+2.0d0*sigma(4)*epsi(4)
                        integ=undemi*integ*poids
                    endif
!
                    wtotal=wtotal+integ
!
                else
!
                    wtotal = wtotal + (enelas + eplast)*poids
!
                endif
!
!  ===============================================
!  = FIN TRAITEMENT DE L'OPTION ENER_TOTALE      =
!  ===============================================
!
                goto 10
!
            endif
!
!---------------------------------------------------------
!   CACUL DU TERME PSI REPRESENTANT L'ENERGIE ELASTIQUE  -
!   NON-LINEAIRE TOTALE ASSOCIEE A LA COURBE DE TRACTION -
!---------------------------------------------------------
!
! --- COEFFICIENTS DU MATERIAU ( DE LAME : MU ET MODULE DE
! --- COMPRESSIBILITE : K) :
!
            deuxmu = e/(un+nu)
            k = untier*e/(un - deux*nu)
!
            eplast = zero
            p = zero
!
!
!  -- DEFORMATION THERMIQUE AU POINT D'INTEGRATION COURANT :
!
            call verift(fami, igau, 1, '+', zi(imate),&
                        epsth=epsthe)
!
!
! --- TRAITEMENT DU CAS CONTRAINTES PLANES :
!
            if (lteatt('C_PLAN','OUI')) then
!
! --- CALCUL DE LA COMPOSANTE EPSZZ DE LA DEFORMATION TOTALE
! --- EN ECRIVANT EPSZZ = EPSZZ_ELAS   + EPSZZ_PLAS
! --- SOIT        EPSZZ = D-1*SIGMA(3) - EPSXX_PLAS - EPSYY_PLAS:
!
                c1 = (un + nu)/e
                c2 = nu/e
                trsig = sigma(1) + sigma(2)
                epsel(1) = c1*sigma(1) - c2*trsig
                epsel(2) = c1*sigma(2) - c2*trsig
                epsel(3) = - c2*trsig
!
                epsm(3+(igau-1)*nbsig)= epsel(3) + epsthe - epsm(1+(&
                igau-1)*nbsig) + epsel(1) - epsm(2+(igau-1)*nbsig) +&
                epsel(2)
            endif
!
! --- CALCUL DE LA DILATATION VOLUMIQUE AU POINT D'INTEGRATION COURANT:
!
            trepsm = epsm( 1+(igau-1)*nbsig) + epsm(2+(igau-1)*nbsig) + epsm(3+(igau-1)*nbsig )
!
! --- CALCUL DU DEVIATEUR DES DEFORMATIONS AU POINT D'INTEGRATION
! --- COURANT :
!
            epsdv(1) = epsm(1+(igau-1)*nbsig) - untier*trepsm
            epsdv(2) = epsm(2+(igau-1)*nbsig) - untier*trepsm
            epsdv(3) = epsm(3+(igau-1)*nbsig) - untier*trepsm
            epsdv(4) = epsm(4+(igau-1)*nbsig)
!
! --- CALCUL DE LA DEFORMATION ELASTIQUE EQUIVALENTE AU
! --- POINT D'INTEGRATION COURANT :
!
            epseq = sqrt(&
                    trois/deux* (&
                    epsdv(1)*epsdv(1) + epsdv(2)* epsdv(2) + epsdv(3)*epsdv(3) + deux*epsdv(4)*ep&
                    &sdv(4)&
                    )&
                    )
!
! --- CALCUL DE LA CONTRAINTE ELASTIQUE EQUIVALENTE AU
! --- POINT D'INTEGRATION COURANT :
!
            sigeq = deuxmu*epseq
!
! --- PARTIE SPHERIQUE DE L'ENERGIE DE DEFORMATION ELASTIQUE :
!
            enelsp = undemi*k*trepsm*trepsm
!
! --- TRAITEMENT DU CAS DE L'ECROUISSAGE LINEAIRE ISOTROPE :
!
            if (compor(1) .eq. 'VMIS_ISOT_LINE') then
!
! --- RECUPERATION DE LA LIMITE D'ELASTICITE SY
! --- ET DE LA PENTE DE LA COURBE DE TRACTION D_SIGM_EPSI :
!
                nomres(1) = 'D_SIGM_EPSI'
                nomres(2) = 'SY'
!
                call rcvalb(fami, igau, 1, '+', zi(imate),&
                            ' ', 'ECRO_LINE', 0, ' ', [0.d0],&
                            2, nomres, valres, icodre, 2)
!
                dsde = valres(1)
                sigy = valres(2)
!
! --- PENTE DE LA COURBE DE TRACTION DANS LE DIAGRAMME 'REDRESSE' :
!
                rprim = e*dsde/(e-dsde)
!
! --- DEFORMATION NON-LINEAIRE CUMULEE EQUIVALENTE :
!
                p = (sigeq - sigy)/(rprim + trois/deux*deuxmu)
                if (p .le. r8prem()) p = zero
!
! --- CONTRAINTE UNIAXIALE SUR LA COURBE DE TRACTION :
!
                rp = sigy + rprim*p
!
! ---  TRAVAIL ELASTIQUE NON-LINEAIRE 'EQUIVALENT' :
!
                eplast = undemi*(sigy+rp)*p
!
! ---  TRAITEMENT DU CAS DE L'ECROUISSAGE NON-LINEAIRE ISOTROPE :
!
            else if (compor(1).eq.'VMIS_ISOT_TRAC') then
!
! ---  RECUPERATION DE LA COURBE DE TRACTION :
!
                call rctrac(zi(imate), 1, 'SIGM', tempg, jprol,&
                            jvale, nbval, e)
!
! --- CALCUL DE LA LIMITE ELASTIQUE SIGY :
!
                call rcfonc('S', 1, jprol, jvale, nbval,&
                            sigy = sigy)
!
                if (sigeq .ge. sigy) then
!
! --- CALCUL DU TRAVAIL ELASTIQUE NON-LINEAIRE ET DE LA
! --- CONTRAINTE EQUIVALENTE :
!
                    call rcfonc('E', 1, jprol, jvale, nbval,&
                                e = e, nu = nu, p = zero, rp = rp, rprim = rprim,&
                                airerp = airep, sieleq = sigeq, dp = p)
!
! --- TRAVAIL ELASTIQUE NON-LINEAIRE 'EQUIVALENT' :
!
                    eplast = airep
                endif
            else
                call utmess('F', 'ELEMENTS4_2')
            endif
!
! --- PARTIE DEVIATORIQUE DE L'ENERGIE DE DEFORMATION ELASTIQUE
! --- TOTALE 'EQUIVALENTE' (I.E. ASSOCIEE A LA COURBE DE
! --- TRACTION SI ON CONSIDERAIT LE MATERIAU ELASTIQUE
! --- NON-LINEAIRE :
!
            if (p .le. r8prem()) then
                eneldv = epseq*epseq*deuxmu/trois
            else
                eneldv = rp*rp/deuxmu/trois
            endif
!
! --- ENERGIE DE DEFORMATION ELASTIQUE TOTALE AU POINT
! --- D'INTEGRATION COURANT :
!
            enelto = enelsp + eneldv + eplast
!
! --- AFFECTATION A PSI QUI EST L'ENERGIE ELASTIQUE TOTALE
! --- DE LA CONTRIBUTION AU POINT D'INTEGRATION DE CETTE ENERGIE :
!
            psi = psi + enelto
!
! --- VOLUME DE L'ELEMENT :
!
            volume = volume + poids
!
! --- INDICATEUR GLOBAL ENERGETIQUE (NON NORMALISE) :
!
            if (omega .ge. 1d04*r8prem()) then
                if (psi .lt. omega) then
                    indigl = indigl + (un - psi/omega)*poids
                endif
            endif
!
 10     continue
!
! ----   RECUPERATION ET AFFECTATION DES GRANDEURS EN SORTIE
! ----   AVEC RESPECTIVEMENT LA VALEUR DE L'INDICATEUR GLOBAL SUR
! ----   L'ELEMENT ET LE VOLUME DE L'ELEMENT POUR L'OPTION
! ----   INDIC_ENER
! ----   AFFECTATION DE L'ENERGIE DE DEFORMATION ELASTIQUE
! ----   ET DE L'ENERGIE DE DEFORMATION TOTALE RESPECTIVEMENT
! ----   POUR LES OPTIONS ENEL_ELEM ET ENER_TOTALE :
!
        if (option .eq. 'INDIC_ENER') then
            call jevech('PENERD1', 'E', idene1)
            zr(idene1) = indigl
            call jevech('PENERD2', 'E', idene2)
            zr(idene2) = volume
        else if (option.eq.'ENEL_ELEM') then
            call jevech('PENERD1', 'E', idene1)
            zr(idene1) = welas
        else if (option.eq.'ENER_TOTALE') then
            call jevech('PENERD1', 'E', idene1)
            zr(idene1) = wtotal
        endif
!
!  ===========================
!  =                         =
!  = OPTION   "INDIC_SEUIL"  =
!  =                         =
!  ===========================
!
    else if (option.eq.'INDIC_SEUIL') then
!
! --- BOUCLE SUR LES POINTS D'INTEGRATION
!
        do 60 igau = 1, npg
!
!
!
! --- RECUPERATION DES CARACTERISTIQUES DU MATERIAU :
!
            nomres(1) = 'E'
            nomres(2) = 'NU'
!
            call rcvalb(fami, igau, 1, '+', zi(imate),&
                        ' ', 'ELAS', 0, ' ', [0.d0],&
                        2, nomres, valres, icodre, 2)
!
            e = valres(1)
            nu = valres(2)
!
! --- TENSEUR DES CONTRAINTES AU POINT D'INTEGRATION COURANT :
!
            do 70 i = 1, nbsig
                sigma(i) = zr(idsig+(igau-1)*nbsig+i-1)
 70         continue
!
! --- CALCUL DES DEFORMATIONS ELASTIQUES AU POINT
! --- D'INTEGRATION COURANT EN CONSIDERANT LE MATERIAU ISOTROPE :
! --- EPS_ELAS    = 1/D*SIGMA
! ---             = ((1+NU)/E)*SIGMA-(NU/E)*TRACE(SIGMA) :
!
            c1 = (un + nu)/e
            c2 = nu/e
!
! --- CAS DES CONTRAINTES PLANES :
!
            if (lteatt('C_PLAN','OUI')) then
                trsig = sigma(1) + sigma(2)
                epsel(1) = c1*sigma(1)-c2*trsig
                epsel(2) = c1*sigma(2)-c2*trsig
                epsel(3) = -c2*trsig
                epsel(4) = c1*sigma(4)
!
! --- CAS AXI ET DEFORMATIONS PLANES :
!
            else
                trsig = sigma(1) + sigma(2) + sigma(3)
                epsel(1) = c1*sigma(1)-c2*trsig
                epsel(2) = c1*sigma(2)-c2*trsig
                epsel(3) = c1*sigma(3)-c2*trsig
                epsel(4) = c1*sigma(4)
            endif
!
! --- CALCUL DES DEFORMATIONS PLASTIQUES AU POINT
! --- D'INTEGRATION COURANT
! --- EPS_PLAST = EPS_TOT - EPS_ELAS - EPSTH :
! --- EPS_PLAST = EPS_HORS_THERMIQUE - EPS_ELAS  :
!
            epspla(1) = epsm(1+(igau-1)*nbsig) - epsel(1)
            epspla(2) = epsm(2+(igau-1)*nbsig) - epsel(2)
            epspla(3) = epsm(3+(igau-1)*nbsig) - epsel(3)
            epspla(4) = epsm(4+(igau-1)*nbsig) - epsel(4)
!
! --- CAS DE L'ECROUISSAGE CINEMATIQUE :
! --- LE TENSEUR A PRENDRE EN CONSIDERATION POUR LE CALCUL
! --- DU TRAVAIL PLASTIQUE EST SIGMA - X OU X EST LE TENSEUR DE RAPPEL :
!
            if (compor(1) .eq. 'VMIS_CINE_LINE') then
                nbsig2 = 7
                ASSERT(idvari.ne.1)
                do 75 i = 1, nbsig
                    x(i) = zr(idvari+(igau-1)*nbsig2+i-1)
 75             continue
                do 80 i = 1, nbsig
                    sigma(i) = sigma(i) - x(i)
 80             continue
            endif
!
! --- CALCUL DU TRAVAIL PLASTIQUE AU POINT D'INTEGRATION COURANT :
!
            eplast = sigma(1)*epspla(1) + sigma(2)*epspla(2) + sigma( 3)*epspla(3) + deux*sigma(4&
                     &)*epspla(4)
! VALEUR ABSOLUE DU TRAVAIL PLASTIQUE
            eplast=abs(eplast)
! --- CALCUL DU TRAVAIL PLASTIQUE EQUIVALENT AU POINT
! --- D'INTEGRATION COURANT :
!
! --- TRAITEMENT DU CAS DE L'ECROUISSAGE LINEAIRE  :
!
            if (compor(1) .eq. 'VMIS_ISOT_LINE' .or. compor(1) .eq. 'VMIS_CINE_LINE') then
!
! --- RECUPERATION DE LA LIMITE D'ELASTICITE SY
! --- ET DE LA PENTE DE LA COURBE DE TRACTION D_SIGM_EPSI :
!
                nomres(1) = 'D_SIGM_EPSI'
                nomres(2) = 'SY'
                call rcvalb(fami, igau, 1, '+', zi(imate),&
                            ' ', 'ECRO_LINE', 0, ' ', [0.d0],&
                            2, nomres, valres, icodre, 2)
!
                dsde = valres(1)
                sigy = valres(2)
!
! --- CALCUL DE LA DEFORMATION PLASTIQUE EQUIVALENTE :
!
                epseq = sqrt(&
                        trois/deux*(&
                        epspla(1)*epspla(1) + epspla(2)*epspla(2) + epspla(3)*epspla(3) + deux* e&
                        &pspla(4)*epspla(4)&
                        )&
                        )
!
! --- DEFORMATION PLASTIQUE CUMULEE :
!
! --- (TEMPORAIRE POUR L'ECROUISSAGE CINEMATIQUE)
                if (compor(1) .eq. 'VMIS_CINE_LINE') then
                    p = epseq
                else if (compor(1).eq.'VMIS_ISOT_LINE') then
                    p = zr(idvari+(igau-1)*nbvari+1-1)
                endif
!
! --- PENTE DE LA COURBE DE TRACTION DANS LE DIAGRAMME 'REDRESSE' :
!
                rprim = e*dsde/(e-dsde)
!
! --- CONTRAINTE UNIAXIALE SUR LA COURBE DE TRACTION :
!
                rp = sigy + rprim*p
!
! --- TRAVAIL PLASTIQUE 'EQUIVALENT' :
!
                eplaeq = rp*p
!
! --- TRAITEMENT DU CAS DE L'ECROUISSAGE NON-LINEAIRE ISOTROPE :
!
            else if (compor(1).eq.'VMIS_ISOT_TRAC') then
!
! --- RECUPERATION DE LA COURBE DE TRACTION :
!
                call rcvarc(' ', 'TEMP', '+', fami, igau,&
                            1, tempg, iret1)
                call rctrac(zi(imate), 1, 'SIGM', tempg, jprol,&
                            jvale, nbval, e)
!
! --- RECUPERATION DE LA DEFORMATION PLASTIQUE CUMULEE :
!
                p = zr(idvari+(igau-1)*nbvari+1-1)
!
! --- TRAVAIL PLASTIQUE 'EQUIVALENT' :
!
                call rcfonc('V', 1, jprol, jvale, nbval,&
                            p = p, rp = rp, rprim = rprim, airerp = airep)
!
                eplaeq = rp*p
            else
                call utmess('F', 'ELEMENTS4_3')
            endif
!
! --- CALCUL DU JACOBIEN AU POINT D'INTEGRATION COURANT :
!
            call nmgeom(2, nno, axi, grand, zr(igeom),&
                        igau, ipoids, ivf, idfde, zr(idepl),&
                        .true._1, poids, trav, f, eps,&
                        r)
!
! --- VOLUME DE L'ELEMENT :
!
            volume = volume + poids
!
! --- INDICATEUR GLOBAL ENERGETIQUE (NON NORMALISE) :
!
            if (eplaeq .ge. 1d04*r8prem()) then
                indigl = indigl + (un - eplast/eplaeq)*poids
            endif
!
 60     continue
!
! ---- RECUPERATION ET AFFECTATION DES GRANDEURS EN SORTIE
! ---- AVEC RESPECTIVEMENT LA VALEUR DE L'INDICATEUR GLOBAL SUR
! ---- L'ELEMENT ET LE VOLUME DE L'ELEMENT :
!
        call jevech('PENERD1', 'E', idene1)
        zr(idene1) = indigl
        call jevech('PENERD2', 'E', idene2)
        zr(idene2) = volume
!
    endif
!
end subroutine
