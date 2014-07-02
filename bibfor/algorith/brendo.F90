subroutine brendo(sige6, bt6, sut, bc1, suc,&
                  local, t33, n33, lct, bw,&
                  pw, bch, pch, delta, lcc,&
                  mt, mc, siget6, sigec6, nu,&
                  dt66, dc0, sut6, suc1, siga6,&
                  dt6)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!      CALCUL DES ENDOMMAGEMENTS DE TRACTION ET DE COMPRESSION
!
!      ATTENTION : UTILISATION DES SUBROUTINES
!     ZERO,MATMAT...
!
! aslint: disable=W1504
    implicit none
#include "asterf_types.h"
#include "asterfort/brindz.h"
#include "asterfort/brtail.h"
#include "asterfort/brvp33.h"
#include "asterfort/matini.h"
#include "asterfort/transp.h"
#include "asterfort/utbtab.h"
    integer :: i, j, k, l
    real(kind=8) :: sige6(6), sige33(3, 3), sige3(3), vse33(3, 3)
    real(kind=8) :: vse33t(3, 3)
    real(kind=8) :: bt6(6), bt33(3, 3), sut33(3, 3), sut3(3)
    real(kind=8) :: bt3(3), vbt33(3, 3), vbt33t(3, 3)
    real(kind=8) :: sget33(3, 3), sgec33(3, 3)
    aster_logical :: local, lcomp, endoc, aster
    real(kind=8) :: x33(3, 3), y33(3, 3), z33(3, 3), y133(3, 3)
    real(kind=8) :: t33(3, 3)
    real(kind=8) :: n33(3, 3), l3(3)
    real(kind=8) :: sget3(3), sgec3(3)
    real(kind=8) :: siget6(6), sigec6(6)
    real(kind=8) :: nu
    real(kind=8) :: bt033(3, 3), bt033t(3, 3)
    real(kind=8) :: dt66(6, 6), dt6(6)
    real(kind=8) :: s33(3, 3), trav(3, 3)
    real(kind=8) :: sut6(6), siga6(6), siga33(3, 3)
    real(kind=8) :: sigat6(6), dt33(3, 3), dt33f(3, 3)
    real(kind=8) :: lcc, lct, lm, mc, mt
    real(kind=8) :: bc1, bch, bw, bwpw, dc0, delta, e1
    real(kind=8) :: pch, pw, rh, rj2, s1, s2, s3
    real(kind=8) :: suc, suc1, sut, t1, t10, t12, t14
    real(kind=8) :: t18, t2, t22, t23, t3, t5, t6
    real(kind=8) :: t8, xc, xi1, xj2, xs, yc, zz
!
!-----------------------------------------------------------------------
    zz = 0.d0
!
!  *INITIALISATION LOCALISATION ET ENDOMMAGEMENT TRAITES EN COMPRESSION*
    endoc=.true.
    lcomp=.true.
!
!     *****INITIALISATION DE L INDICATEUR DE LOCALISATION *******
!     INDIC=.FALSE.
!
!     ***** ACTIVATION DES CALCULS COMPLEMENTAIRES POUR ASTER ***
    aster=.true.
!
!     T33 ET N33 SONT LES TENSEURS CARACTéRISANT LA TAILLE DES ELEMENTS
!     MASSIFS UTILISéS
!
!      SI LOCAL=.TRUE. ON EFFECTUE UNE HOMOTHETIE POUR CONTROLER
!      L ENERGIE DISSIPEE
!     CETTE HOMOTHETIE NE CONCERNE QUE LA PART DE CONTRAINTE DUE AU
!     CHARGEMENT EXTERNE
!     LA PROCEDURE EST DONC A REVOIR EXTERIEUR
!
!     **** HYPOTHESE D ENDOMMAGEMENT DE TRACTION ANISOTROPE **********
!
!      RANGEMENT DES CONTRAINTES EFFECTIVES EN TABLEAU 3*3
    sige33(1,1)=sige6(1)
    sige33(2,2)=sige6(2)
    sige33(3,3)=sige6(3)
    sige33(1,2)=sige6(4)
    sige33(1,3)=sige6(5)
    sige33(2,3)=sige6(6)
    sige33(2,1)=sige33(1,2)
    sige33(3,1)=sige33(1,3)
    sige33(3,2)=sige33(2,3)
!
!
!      RANGEMENT DES INDICES ADIMENSIONNELS EN TABLEAU 3*3
    bt33(1,1)=bt6(1)
    bt33(2,2)=bt6(2)
    bt33(3,3)=bt6(3)
    bt33(1,2)=bt6(4)
    bt33(1,3)=bt6(5)
    bt33(2,3)=bt6(6)
    bt33(2,1)=bt33(1,2)
    bt33(3,1)=bt33(1,3)
    bt33(3,2)=bt33(2,3)
!
!     CALCUL DES CONTRAINTES SEUILS DE TRACTION EN DEBUT DE
!       PAS A PARTIR DES INDICES
    call brvp33(bt33, bt3, bt033)
    call transp(bt033, 3, 3, 3, bt033t,&
                3)
!
!     *** ACTUALISATION DES CONTRAINTES SEUILS DE FISSURATION ********
!
!     PRINT*,'INDICE INITIAL EN BASE FIXE'
!      CALL AFFICHE33(BT33)
!      PRINT*,'MATRICE DE PASSAGE à LA BASE D ENDO'
!      CALL AFFICHE33 (VBT33)
!
    call matini(3, 3, zz, x33)
    e1=1.d0/mt
    do i = 1, 3
        if (bt3(i) .gt. 0.d0) then
            x33(i,i)=((mt*bt3(i))**e1)*sut
        else
            x33(i,i)=0.d0
        endif
!      SEUIL3(I)=X33(I,I)
    end do
!     PASSAGE DES CONTRAINTES SEUILS EN BASE FIXE
    call utbtab('ZERO', 3, 3, x33, bt033t,&
                trav, sut33)
!      PRINT*,'SEUIL INITIAL EN BASE FIXE'
!      CALL AFFICHE33(SUT33)
!     CONTRAINTE SEUIL DE COMPRESSION EN DEBUT DE PAS
    if (bc1 .gt. 0.d0) then
        suc1=((mc*bc1)**(1.d0/mc))*suc
    else
        suc1=0.d0
    endif
!
!     CONTRAINTES EFFECTIVES PRINCIPALES ET MATRICE DE
!       PASSAGE à LA BASE PRINCIPALE
    do i = 1, 3
        do j = 1, 3
            x33(i,j)=sige33(i,j)
        end do
    end do
!      DIAGONALISATION ET VALEURS PROPRES PAR LA METHODE DE JACOBI
    call brvp33(x33, sige3, vse33)
!
!      CALL AFFICHE33(X33)
!
!      TRANSPOSITION DE DE LA MATRICE DE PASSAGE
    call transp(vse33, 3, 3, 3, vse33t,&
                3)
!      PRINT*,'PASSAGE A BASE PRIN SIGMA'
!      CALL AFFICHE33(VSE33)
!
!      DECOMPOSITION DES CONTRAINTES
!        PRINCIPALES EN PARTIE POSITIVE ET NéGATIVE
    do i = 1, 3
        sget3(i)=0.5d0*(sige3(i)+abs(sige3(i)))
        sgec3(i)=0.5d0*(sige3(i)-abs(sige3(i)))
    end do
!
!     MISE A JOUR DES INDICES DE FISSURATION DE TRACTION
!     LA MISE A JOUR SE FAIT SUR LES CONTRAINTES SEUILS
!     VALEUR PRINCIPALES DES SEUILS ACTUELS
    do i = 1, 3
        do j = 1, 3
            x33(i,j)=sut33(i,j)
        end do
    end do
!     EXPRESSION DES CONTRAINTES SEUILS DE TRACTION DANS
!       LA BASE PRINCIPALE DES CONTRAINTES EFFECTIVES ACTUELLES
    call utbtab('ZERO', 3, 3, x33, vse33,&
                trav, y133)
!      PRINT*,'SEUIL INITIAUX EN BASE PRINCIPALE'
!      CALL AFFICHE33(Y133)
!
!     CALCUL DE LA TAILLE DES ELEMENTS FINIS DANS LES DIRECTIONS
!       PRINCIPALES DES CONTRAINTES
    if (local) then
        call brtail(l3, t33, n33, vse33)
    endif
!
!    ACTUALISATION DES CONTRAINTES SEUILS(ECOULEMENT/CRITERE DE RANKINE)
    do i = 1, 3
        if (local) then
!        ON TESTE LA LOCALISATION SUR LES CONTRAINTES EFFECTIVES ISSUES
!        DU CHARGEMENT EXTéRIEUR (SEULE CAUSE POSSIBLE DE LOCALISATION)
!       PRINT*,'SGET3(',I,')',SGET3(I),'BW,PW,BCH,PCH',BW,PW,BCH,PCH
            sget3(i)=sget3(i)-bw*pw-bch*pch
            if (sget3(i) .gt. sut) then
!        LE CHARGEMENT ACTUEL PROVOQUE LA LOCALISATION :
!          ON EFFECTUE L HOMOTHETIE
!        INDIC=.TRUE.
                sget3(i)=(sget3(i)-sut)*l3(i)/lct+sut
!         SGET3(I)=(SGET3(I)-SUT)*2.D0+SUT
            endif
!       ON REPASSE EN CONTRAINTE EFFECTIVE INTéRIEURE POUR
!       LA CONFORMITé AVEC LE CALCUL DES CONTRAINTES APPARENTES
            sget3(i)=sget3(i)+bw*pw+bch*pch
        endif
        do j = 1, 3
            if (i .eq. j) then
                y33(i,i)=max(y133(i,i),sget3(i))
            else
                y33(i,j)=y133(i,j)
            endif
        end do
    end do
!      PRINT*,'CONTRAINTES SEUILS EN BASE PRINCIPALE'
!      CALL AFFICHE33(Y33)
!
!     RETOUR DES CONTRAINTES SEUILS DANS LA BASE FIXE
    call utbtab('ZERO', 3, 3, y33, vse33t,&
                trav, z33)
!      PRINT*,'CONTRAINTES SEUILS EN BASE FIXE'
!      CALL AFFICHE33(Z33)
!
!     STOCKAGE DES INDICES DE FISSURATION APRES ECOULEMENT
    call brvp33(z33, sut3, vbt33)
    call transp(vbt33, 3, 3, 3, vbt33t,&
                3)
    call matini(3, 3, zz, x33)
    do i = 1, 3
        if (sut3(i) .gt. 0.d0) then
            bt3(i)=e1*((sut3(i)/sut)**mt)
        else
            bt3(i)=0.d0
        endif
!       PRINT*,'BT3(',I,')=',BT3(I),'DS ENDO'
    end do
!
!     RETOUR DES INDICES DE FISSURATION EN BASE FIXE POUR LE
!       STOCKAGE EN VECTEUR6
    call matini(3, 3, zz, x33)
    call matini(3, 3, zz, dt33)
    do i = 1, 3
        x33(i,i)=bt3(i)
        dt33(i,i)=1.d0-exp(-bt3(i))
    end do
    call utbtab('ZERO', 3, 3, x33, vbt33t,&
                trav, bt33)
    call utbtab('ZERO', 3, 3, dt33, vbt33t,&
                trav, dt33f)
    do i = 1, 3
        bt6(i)=bt33(i,i)
        dt6(i)=dt33f(i,i)
    end do
    bt6(4)=bt33(1,2)
    bt6(5)=bt33(1,3)
    bt6(6)=bt33(2,3)
    dt6(4)=dt33f(1,2)
    dt6(5)=dt33f(1,3)
    dt6(6)=dt33f(2,3)
!
!
!     RETOUR DE CONTRAINTES SEUILS EN
!       BASE FIXE SI STOCKAGE NECESSAIRE (VERSION ASTER)
    if (aster) then
        call matini(3, 3, zz, x33)
        do i = 1, 3
            x33(i,i)=sut3(i)
        end do
        call utbtab('ZERO', 3, 3, x33, vbt33t,&
                    trav, sut33)
        do i = 1, 3
            sut6(i)=sut33(i,i)
        end do
        sut6(4)=sut33(1,2)
        sut6(5)=sut33(1,3)
        sut6(6)=sut33(2,3)
    endif
!
!     *** ACTUALISATION DU TENSEUR DES ENDOMMAGEMENTS DE TRACTION ***
!
!     HYPOTHESE DU MATERIAU ORTHOTROPE DANS LES
!       DIRECTIONS PRINCIPALES DE FISSURATION
!
!     CALCUL DE LA MATRICE CARRéE S33=S0/S1 POUR LES TRACTION EFFECTIVES
    s1=exp(bt3(1))
    s2=exp(bt3(2))
    s3=exp(bt3(3))
    t1 = s2 * s3
    t2 = nu ** 2
    t3 = t2 * s3
    t5 = 2.d0 * t2 * nu
    t6 = t2 * s2
    t8 = s1 * s2
    t10 = s1 * t2
    t12 = 1.d0 / (-t8 * s3 + t10 + t3 + t5 + t6)
    t14 = nu * s2
    t18 = nu * s3
    t22 = s1 * s3
    t23 = s1 * nu
    s33(1,1) = (-t1 + t2 + t3 + t5 + t6) * t12
    s33(1,2) = nu * (t1 - s3 - nu + t14) * t12
    s33(1,3) = nu * (t1 + t18 - nu - s2) * t12
    s33(2,1) = nu * (-s3 - nu + t22 + t23) * t12
    s33(2,2) = (t3 + t5 - t22 + t2 + t10) * t12
    s33(2,3) = nu * (t18 + t22 - s1 - nu) * t12
    s33(3,1) = nu * (-nu - s2 + t23 + t8) * t12
    s33(3,2) = nu * (t14 - s1 - nu + t8) * t12
    s33(3,3) = (t5 + t6 + t10 - t8 + t2) * t12
!
!     CALCUL DE LA MATRICE D ENDOMMAGEMENT DT66
    do i = 1, 6
        do j = 1, 6
            dt66(i,j)=0.d0
        end do
    end do
    do i = 1, 3
        do j = 1, 3
            if (i .eq. j) then
                dt66(i,j)=1.d0-s33(i,j)
            else
                dt66(i,j)=-s33(i,j)
            endif
        end do
    end do
    do i = 4, 6
        call brindz(i, k, l)
        dt66(i,i)=1.d0-exp(-bt3(k)-bt3(l))
    end do
!
!
!     **** CAS DE L ENDOMMAGEMENT ISOTROPE DE COMPRESSION ***********
!
    if (endoc) then
!      MISE A JOUR DE L'ENDOMMAGEMENT ISOTROPE DE
!        COMPRESSION (DRUCKER PRAGGER)
!      CALCUL DES INVARIANTS DU TENSEUR DES CONTRAINTES EFFECTIVES
        xj2=(sgec3(1)-sgec3(2))**2+(sgec3(1)-sgec3(3))**2 +(sgec3(2)-&
        sgec3(3))**2
        rj2=sqrt(xj2/6.d0)
        xi1=(sgec3(1)+sgec3(2)+sgec3(3))/3.d0
!      CONTRAINTE EQUIVALENTE
        xc=(rj2+delta*xi1)
        xc=0.5d0*(xc+abs(xc))
!
!      SEUIL ACTUEL
        xs=suc1
!      TRAITEMENT DE LA LOCALISATION EN COMPRESSION : TEST SUR
!      LES CONTRAINTES EFFECTIVES ISSUES DU CHARGEMENT EXTéRIEUR
        if (local .and. lcomp) then
            if (xc .gt. suc) then
!         LA LOCALISATION EN COMPRESSION EST EN COURS
!         INDIC=.TRUE.
!         LONGUEUR EQUIVALENTE
                lm=(l3(1)*l3(2)*l3(3))**(1.d0/3.d0)
!         HOMOTHETIE SUR LES CONTRAINTES DEVIATORIQUES NEGATIVES
                yc=(xc-suc)*lm/lcc+suc
!      RAPPORT D HOMOTHETIE ISOTROPES DES CONTRAINTES EXTERNES NEGATIVES
                rh=yc/xc
                xc=yc
!         ACTUALISATION DES CONTRAINTES EFFECTIVES EN ZONE DE
!           LOCALISATION SUIVANT LE RAPPORT D HOMOTHETIE
                do i = 1, 3
!          CAS DES CONTRAINTES DE COMPRESSION
!            (LOCALISATION EN COMPRESSION ISOTROPE)
                    sgec3(i)=rh*(sgec3(i)-bw*pw-bch*pch)+(bw*pw+bch*&
                    pch)
                end do
!         ESTIMATION DE L ENDOMMAGEMENT DE COMPRESSION APRES HOMOTHETIE
!         DES CONTRAINTES DUES AU CHARGEMENT EXT
                xj2=(sgec3(1)-sgec3(2))**2+(sgec3(1)-sgec3(3))**2&
                +(sgec3(2)-sgec3(3))**2
                rj2=sqrt(xj2/6.d0)
                xi1=(sgec3(1)+sgec3(2)+sgec3(3))/3.d0
!         NOUVELLE CONTRAINTE EQUIVALENTE APRES HOMOTHETIE
                xc=(rj2+delta*xi1)
                xc=0.5d0*(xc+abs(xc))
            endif
        endif
!      ACTUALISATION DU SEUIL DE COMPRESSION
        xc=max(xc,xs)
        suc1=xc
    endif
!     ACTUALISATION DE L'INDICE DE FISSURATION DE DRUCKER PRAGER
    if (suc1 .gt. 0.d0) then
        bc1=((suc1/suc)**mc)/mc
    else
        bc1=0.d0
    endif
!     ENDOMMAGEMENT DE COMPRESSION
    dc0=1.d0-exp(-bc1)
!
!      PRINT*,'BT33 EN BASE FIXE :'
!      CALL AFFICHE33(BT33)
!      PRINT*,'BC1:',BC1
!      READ*
!
!
!     CALCUL DES CONTRAINTES EFFECTIVES EN BASE PRINCIPALE D ENDOMMAGEMT
!
!
!
!     PASSAGE DES CONTRAINTES PRINCIPALES DE
!       TRACTION ACTUALISEES EN BASE ENDO TRACT
    call matini(3, 3, zz, x33)
    do i = 1, 3
        x33(i,i)=sget3(i)
    end do
    call utbtab('ZERO', 3, 3, x33, vse33t,&
                trav, sget33)
!     PASSAGE EN BASE PRIN ENDO TRAC
    call matini(3, 3, zz, x33)
    call utbtab('ZERO', 3, 3, sget33, vbt33,&
                trav, x33)
    do i = 1, 6
        call brindz(i, j, k)
        siget6(i)=x33(j,k)
    end do
!
!     PASSAGE DES CONTRAINTES PRINCIPALES DE COMPRESSION
!       ACTUALISEES EN BASE ENDO TRACT
    call matini(3, 3, zz, x33)
    do i = 1, 3
        x33(i,i)=sgec3(i)
    end do
    call utbtab('ZERO', 3, 3, x33, vse33t,&
                trav, sgec33)
!     PASSAGE EN BASE PRIN ENDO TRAC
    call matini(3, 3, zz, x33)
    call utbtab('ZERO', 3, 3, sgec33, vbt33,&
                trav, x33)
    do i = 1, 6
        call brindz(i, j, k)
        sigec6(i)=x33(j,k)
    end do
!
!
!     *** CONTRAINTES APPARENTES ********************************
!
!      DO I=1,6
!       SIGA6(I)=0.D0
!       DO J=1,6
!        IF((I.LE.3).AND.(J.EQ.I))THEN
!        ON EST SUR LA DIAGONALE DU TENSEUR DES CONTRAINTES
!         IF(PW.GT.0.)THEN
!         CAS DE LA SURPRESSION HYDRIQUE APPLIQUéE DANS LES FISSURES
!          SIGA6(I)=SIGA6(I)+(1.-DC0)*
!     #    ((1.-DT66(I,I))*SIGET6(I)+SIGEC6(I))-BW*PW
!         ELSE
!         LA DEPRESSION NE S APPLIQUE PAS DANS LES FISSURES
!          BWEQ=BW*(1.-DC0)*(1.-DT66(I,I))
!          SIGA6(I)=SIGA6(I)+(1.-DC0)*
!     #    ((1.-DT66(I,I))*SIGET6(I)+SIGEC6(I))-BWEQ*PW
!         ENDIF
!        ELSE
!         IF(J.EQ.I) THEN
!          SIGA6(I)=SIGA6(I)+(1.-DC0)*
!     #    ((1.-DT66(I,I))*SIGET6(I)+SIGEC6(I))
!         ELSE
!          SIGA6(I)=SIGA6(I)-DT66(I,J)*(1.-DC0)*SIGET6(J)
!         ENDIF
!        ENDIF
!       END DO
!      END DO
!
!     CALCUL DES CONTRAINTES APPARENTES DE TRACTION EN
!       BASE PRINCIPALE D ENDOMMAGEMENT
    do i = 1, 6
        sigat6(i)=0.d0
        do j = 1, 6
            if (j .eq. i) then
!        ON EST SUR LA DIAGONALE
                if (i .le. 3) then
!         ON EST SUR LA CONTRAINTE NORMALE : ON RAJOUTE
!           LA PRESSION AUX CONTRAINTES DE TRACTION UNIQUEMENT
                    if (sigec6(i) .lt. 0.d0) then
                        bwpw=0.d0
                    else
                        bwpw=bw*pw
                    endif
!        CHARGEMENT PAR LES PRESSIONS
                    if (bwpw .lt. 0.d0) then
!        CAS DE LA DEPRESSION CAPILLAIRE APPLIQUéE SUR LE SQUELETTE SAIN
                        sigat6(i)=sigat6(i)+(1.d0-dt66(i,j))* (siget6(&
                        j)-bwpw-bch*pch)
                    else
!        LA SURPRESSION  D EAU EXISTE AUSSI DANS
!          LES FISSURES (PAS CELLE DE RAG)
                        sigat6(i)=sigat6(i)+(1.d0-dt66(i,j))* (siget6(&
                        j)-bch*pch)-bwpw
                    endif
                else
!         ON EST PAS LES CONTRAINTES NORMALES MAIS SUR LES CISAILLEMENTS
                    sigat6(i)=sigat6(i)+(1.d0-dt66(i,j))*siget6(j)
                endif
            else
!        ON PREND EN COMPTE LES COUPLAGES AVEC
!          LES CONTRAINTES HOR DIAGONALE
                sigat6(i)=sigat6(i)-dt66(i,j)*siget6(j)
            endif
        end do
!      PASSAGE A LA CONTRAINTE SUIVANTE
    end do
!
!      CALCUL DES CONTRAINTES APPARENTES  TOTALES EN
!        BASE PRINCIPALE D ENDOMMAGEMENT
!      (COUPLAGE AVEC L ENDOMMAGEMENT DE COMPRESSION ET
!        PRISE EN COMPTE DES PRESSIONS
!        HYDRIQUES POUR LES CONTRAINTES NORMALES EFFECTIVES NEGATIVE)
    do i = 1, 6
        if ((sigec6(i).lt.0.d0) .and. (i.le.3)) then
!        LA CONTRAINTE EFFECTIVE NORMALE DANS LA
!          DIRECTION I EST NEGATIVE : ON RAJOUTE LA PRESSION
            if (pw .lt. 0.d0) then
!         DANS LE SQUELETTE SAIN SI DEPRESSION
                siga6(i)=(1.d0-dc0)*(sigat6(i)+sigec6(i)-bw*pw)
            else
!         PARTOUT SI SURPRESSION
                siga6(i)=(1.d0-dc0)*(sigat6(i)+sigec6(i))-bw*pw
            endif
        else
!        ON EST PAS SUR LES CONTRAINTES NORMALES : PAS DE PRESSION
!          ET ENDO DE COMP ISOTROPE
            siga6(i)=(1.d0-dc0)*(sigat6(i)+sigec6(i))
        endif
    end do
!
!     RETOUR DES CONTRAINTES APPARENTES EN BASE FIXE
    call matini(3, 3, zz, x33)
    do i = 1, 6
        call brindz(i, j, k)
        x33(j,k)=siga6(i)
        if (j .ne. k) then
            x33(k,j)=x33(j,k)
        endif
    end do
    call utbtab('ZERO', 3, 3, x33, vbt33t,&
                trav, siga33)
    do i = 1, 6
        call brindz(i, j, k)
        siga6(i)=siga33(j,k)
    end do
!
!
end subroutine
