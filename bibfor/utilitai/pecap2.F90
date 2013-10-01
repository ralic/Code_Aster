subroutine pecap2(chgeoz, iy, iz, s, alpha,&
                  xg, yg, temp1z, temp2z, ay,&
                  az, ey, ez, pctx, pcty)
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
!.======================================================================
    implicit none
!
!      PECAP2  -- DETERMINATION : .DU CENTRE DE TORSION/CISAILLEMENT
!                                 .DES COEFFICIENTS DE CISAILLEMENT
!
!          .LE DOMAINE SUR-LEQUEL ON TRAVAILLE REPRESENTE LA
!           SECTION DE LA POUTRE MAILLEE AVEC DES ELEMENTS 2D
!           ISOPARAMETRIQUES THERMIQUES (THERMIQUES CAR ON
!           DOIT RESOUDRE DES EQUATIONS DE LAPLACE).
!
!          .LES COEFFICIENTS DE CISAILLEMENT AY ET AZ SONT
!           DETERMINES EN FAISANT RESPECTIVEMENT LA RESOLUTION
!           DE L' EQUATION  1 :
!                G*LAPLACIEN(PSI_Z) = -Z*TZ/IY     DANS LA SECTION
!       AVEC     D(PSI_Z )/DN = 0     SUR LE CONTOUR DE LA SECTION
!       ET       PSI_Z = 0    EN UN NOEUD ARBITRAIRE DE LA SECTION
!
!           ET DE L' EQUATION 2 :
!                G*LAPLACIEN(PSI_Y) = -Y*TY/IZ     DANS LA SECTION
!       AVEC     D(PSI_Y )/DN = 0     SUR LE CONTOUR DE LA SECTION
!       ET       PSI_Y = 0    EN UN NOEUD ARBITRAIRE DE LA SECTION
!
!               AY = 2*S*U1_Y/TY**2
!               AZ = 2*S*U1_Z/TZ**2
!       AVEC U1_Y = 0.5*SOMME_S(G*(GRAD(PSI_Y)**2).DS)
!       AVEC U1_Z = 0.5*SOMME_S(G*(GRAD(PSI_Z)**2).DS)
!
!          X DESIGNE L'AXE DE LA POUTRE
!          Y ET Z DESIGNENT LES AXES PRINCIPAUX D'INERTIE DE LA SECTION
!          L'ORIGINE EST SITUEE AU CENTRE DE GRAVITE DE LA SECTION
!          N DESIGNE LE VECTEUR NORMAL A LA FRONTIERE
!
!         TY ET TZ DESIGNENT LES EFFORTS TRANCHANTS
!         ON PREND TY = 1 ET TZ = 1
!         G EST LE MODULE DE CISAILLEMENT
!         ON FAIT L'HYPOTHESE QUE LE MATERIAU EST ISOTROPE
!         DANS CE CAS G N'INTERVIENT PAS
!         IY ET IZ SONT LES MOMENTS D'INERTIE PRINCIPAUX PAR-RAPPORT
!         AUX AXES Y ET Z
!
!          .LES COORDONNEES DU CENTRE DE TORSION/CISAILLEMENT
!           SONT EGALES A :
!             EY =  MX0_Y/TZ
!             EZ = -MX0_Z/TY
!
!           AVEC MX0_Y = SOMME_S((SIGMA_XZ*Y - SIGMA_XY*Z).DS)
!           SACHANT QUE SIGMA_XY = G*D(PSI_Z)/DY
!                   ET  SIGMA_XZ = G*D(PSI_Z)/DZ
!
!           ET  MX0_Z = SOMME_S((SIGMA_XZ*Y - SIGMA_XY*Z).DS)
!           SACHANT QUE SIGMA_XY = G*D(PSI_Y)/DY
!                   ET  SIGMA_XZ = G*D(PSI_Y)/DZ
!
!     OPTION : 'CARA_CISA'
!
!   ARGUMENT        E/S  TYPE         ROLE
!    CHGEOZ         IN    K*      COORDONNEES DES CONNECTIVITES
!                                 DANS LE REPERE PRINCIPAL D'INERTIE
!    IY             IN    R       MOMENT D'INERTIE PRINCIPAL/Y
!    IZ             IN    R       MOMENT D'INERTIE PRINCIPAL/Z
!    S              IN    R       SURFACE DE LA SECTION
!    ALPHA          IN    R       ANGLE FAISANT PASSER DU REPERE
!                                 PRINCIPAL D'INERTIE AU REPERE GLOBAL
!    XG             IN    R       ABSCISSE DU CENTRE DE GRAVITE DE
!                                 LA SECTION DANS LE REPERE GLOBAL
!    YG             IN    R       ORDONNEE DU CENTRE DE GRAVITE DE
!                                 LA SECTION DANS LE REPERE GLOBAL
!    TEMP1Z         IN    K*      RESULTAT DE TYPE EVOL_THER
!                                 REFERENCANT LE CHAMP DE SCALAIRES
!                                 SOLUTION DE L'EQUATION 2
!    TEMP2Z         IN    K*      RESULTAT DE TYPE EVOL_THER
!                                 REFERENCANT LE CHAMP DE SCALAIRES
!                                 SOLUTION DE L'EQUATION 1
!    AY             OUT   R       INVERSE DU COEFFICIENT DE
!                                 CISAILLEMENT DEFINI CI-DESSUS
!    AZ             OUT   R       INVERSE DU COEFFICIENT DE
!                                 CISAILLEMENT DEFINI CI-DESSUS
!    EY             OUT   R       COORDONNEE SELON Y DU CENTRE DE
!                                 CISAILLEMENT/TORSION DANS LE REPERE
!                                 PRINCIPAL D'INERTIE
!    EZ             OUT   R       COORDONNEE SELON Z DU CENTRE DE
!                                 CISAILLEMENT/TORSION DANS LE REPERE
!                                 PRINCIPAL D'INERTIE
!    PCTX           OUT   R       ABSCISSE DU CENTRE DE
!                                 CISAILLEMENT/TORSION DANS LE REPERE
!                                 GLOBAL
!    PCTY           OUT   R       ORDONNEE DU CENTRE DE
!                                 CISAILLEMENT/TORSION DANS LE REPERE
!                                 GLOBAL
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
#include "asterc/r8dgrd.h"
#include "asterfort/calcul.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/mesomm.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsutnu.h"
#include "asterfort/utmess.h"
    character(len=*) :: chgeoz, temp1z, temp2z
    real(kind=8) :: iy, iz
! -----  VARIABLES LOCALES
    character(len=8) :: lpain(3), lpaout(1)
    character(len=8) :: tempe1, tempe2
    character(len=8) :: crit, modele
    character(len=19) :: prchno
    character(len=14) :: typre1, typre2
    character(len=19) :: knum1, knum2, ligrth
    character(len=24) :: lchin(3), lchout(1), chgeom
    character(len=24) :: chtem1, chtem2
    real(kind=8) :: work(9), p(2, 2)
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
! ---- INITIALISATIONS
!      ---------------
!-----------------------------------------------------------------------
    integer :: i, ibid, ierd, iret, nbordr
    real(kind=8) :: alpha, alphar, ay, az, ey, ez, pctx
    real(kind=8) :: pcty, prec, s, xg, yg, zero
!
!-----------------------------------------------------------------------
    zero = 0.0d0
    prec = 1.0d-3
    chgeom = chgeoz
    tempe1 = temp1z
    tempe2 = temp2z
    knum1 = '&&PECAP2.NUME_ORD_1'
    knum2 = '&&PECAP2.NUME_ORD_2'
    crit = 'RELATIF'
!
    do i = 1, 9
        work(i) = zero
    end do
!
! --- ON VERIFIE QUE LES RESULTATS SONT DE TYPE EVOL_THER :
!     ---------------------------------------------------
    call dismoi('F', 'TYPE_RESU', tempe1, 'RESULTAT', ibid,&
                typre1, ierd)
    if (typre1 .ne. 'EVOL_THER') then
        call utmess('F', 'UTILITAI3_54')
    endif
!
    call dismoi('F', 'TYPE_RESU', tempe2, 'RESULTAT', ibid,&
                typre2, ierd)
    if (typre2 .ne. 'EVOL_THER') then
        call utmess('F', 'UTILITAI3_55')
    endif
!
! --- RECUPERATION DU NOMBRE D'ORDRES DES RESULTATS :
!     ---------------------------------------------
    call rsutnu(tempe1, ' ', 0, knum1, nbordr,&
                prec, crit, iret)
    if (nbordr .ne. 1) then
        call utmess('F', 'UTILITAI3_56', sk=tempe1)
    endif
!
    call rsutnu(tempe2, ' ', 0, knum2, nbordr,&
                prec, crit, iret)
    if (nbordr .ne. 1) then
        call utmess('F', 'UTILITAI3_56', sk=tempe2)
    endif
!
! --- RECUPERATION DES CHAMPS DE TEMPERATURES DES RESULTATS :
!     -----------------------------------------------------
    call rsexch('F', tempe1, 'TEMP', 0, chtem1,&
                iret)
    call rsexch('F', tempe2, 'TEMP', 0, chtem2,&
                iret)
!
! --- RECUPERATION DU NUME_DDL ASSOCIE AU CHAMP DE TEMPERATURES :
!     ---------------------------------------------------------
    call dismoi('F', 'PROF_CHNO', chtem1, 'CHAM_NO', ibid,&
                prchno, ierd)
!
! --- RECUPERATION DU MODELE ASSOCIE AU NUME_DDL  :
!     ------------------------------------------
    call dismoi('F', 'NOM_MODELE', prchno, 'PROF_CHNO', ibid,&
                modele, ierd)
!
! --- RECUPERATION DU LIGREL DU MODELE  :
!     --------------------------------
    call dismoi('F', 'NOM_LIGREL', modele, 'MODELE', ibid,&
                ligrth, ierd)
!
! --- CALCUL POUR CHAQUE ELEMENT DE LA SECTION DES 4 INTEGRALES
! --- SUIVANTES PERMETTANT LA DETERMINATION DU CENTRE DE
! --- TORSION/CISAILLEMENT ET DES COEFFICIENTS DE CISAILLEMENT :
! ---   I1 = SOMME_S_ELEMENT((D(PSI_Y)/DZ*Y - D(PSI_Y)/DY*Z).DS)
! ---   I2 = SOMME_S_ELEMENT((D(PSI_Z)/DZ*Y - D(PSI_Z)/DY*Z).DS)
! ---   I3 = SOMME_S_ELEMENT((D(PSI_Y)/DY**2 + D(PSI_Y)/DZ**2).DS)
! ---   I4 = SOMME_S_ELEMENT((D(PSI_Z)/DY**2 + D(PSI_Z)/DZ**2).DS)
!       ----------------------------------------------------------
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    lpain(2) = 'PTEMPE1'
    lchin(2) = chtem1
    lpain(3) = 'PTEMPE2'
    lchin(3) = chtem2
    lpaout(1) = 'PCASECT'
    lchout(1) = '&&PECAP2.INTEG'
!
    call calcul('S', 'CARA_CISA', ligrth, 3, lchin,&
                lpain, 1, lchout, lpaout, 'V',&
                'OUI')
!
! --- SOMMATION DES INTEGRALES PRECEDENTES SUR LA SECTION DE LA POUTRE
! ---   WORK(1) = SOMME_SECTION((D(PSI_Z)/DZ*Y - D(PSI_Z)/DY*Z).DS)
! ---   WORK(2) = SOMME_SECTION((D(PSI_Y)/DZ*Y - D(PSI_Y)/DY*Z).DS)
! ---   WORK(3) = SOMME_SECTION((D(PSI_Z)/DY**2 + D(PSI_Z)/DZ**2).DS)
! ---   WORK(4) = SOMME_SECTION((D(PSI_Y)/DY**2 + D(PSI_Y)/DZ**2).DS)
!       ------------------------------------------------------------
    call mesomm(lchout(1), 9, vr=work)
    ey = work(1)/iy
    ez = work(2)/iz
    ay = work(4)*s/iz/iz
    az = work(3)*s/iy/iy
!
! --- PASSAGE DE L'ANGLE FORME PAR LES AXES GLOBAUX AVEC LES AXES
! --- PRINCIPAUX D'INERTIE DE DEGRES EN RADIANS :
!     -----------------------------------------
    alphar = alpha*r8dgrd()
!
! --- CONSTITUTION DE LA MATRICE DE PASSAGE DU REPERE GLOBAL
! --- AU REPERE D'INERTIE :
!     -------------------
    p(1,1) = cos(alphar)
    p(2,1) = -sin(alphar)
    p(1,2) = sin(alphar)
    p(2,2) = cos(alphar)
!
! --- VALEURS DES COORDONNEES DU CENTRE DE TORSION DANS LE REPERE
! --- GLOBAL INITIAL :
!     --------------
    pctx = p(1,1)*ey + p(2,1)*ez + xg
    pcty = p(1,2)*ey + p(2,2)*ez + yg
!
    call detrsd('CHAMP_GD', '&&PECAP2.INTEG')
!.============================ FIN DE LA ROUTINE ======================
end subroutine
