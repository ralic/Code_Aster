subroutine lipsrb(nomres, matprj, sst1, sst2, intf1,&
                  intf2, lino1, lino2, indin1, indin2,&
                  ddlmas, ddlsla, nbmoma, nbmosl, imast,&
                  tramod)
    implicit none
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
!    M. CORUS     DATE 04/02/10
!-----------------------------------------------------------------------
!  BUT:      < CALCUL DE LA MATRICE DE PROJECTION >
!
!  CALCULER LA MATRICE DE PROJECTION POUR LE CAS DES INTERFACES
!  INCOMPATIBLES EN TENANT COMPTE DE L'ORIENTATION DES SOUS-STRUCTURES.
!  GESTION DES LIAISONS INCOMPATIBLES
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
! NOMRES   /I/: NOM UTILISATEUR DU RESULTAT
! MATPRJ   /O/: MATRICE DE PROJECTION
! SST1     /I/: NOM UTILISATEUR DE LA SOUS STRUCTURE 1
! SST2     /I/: NOM UTILISATEUR DE LA SOUS STRUCTURE 2
! INTF1    /I/: NOM UTILISATEUR DE L'INTERFACE 1
! INTF2    /I/: NOM UTILISATEUR DE L'INTERFACE 2
! LINO1 /I/: VECTEUR CONTENANT LA LISTE DES NOEUDS D'INTEFACE 1
! LINO2 /I/: VECTEUR CONTENANT LA LISTE DES NOEUDS D'INTEFACE 2
! INDIN1 /I/: VECTEUR CONTENANT LES INDICES ASSOCIES AUX DDL
!                 D'INTERFACE 1
! INDIN2 /I/: VECTEUR CONTENANT LES INDICES ASSOCIES AUX DDL
!                 D'INTERFACE 2
! DDLMAS  /I/: NOMBRE DE DDL ACTIFS DE L'INTERFACE MAITRE
! DDLSLA  /I/: NOMBRE DE DDL ACTIFS DE L'INTERFACE ESCLAVE
! NBMOMA /I/: NOMBRE DE MODES DE L'INTERFACE MAITRE
! NBMOSL /I/: NOMBRE DE MODES DE L'INTERFACE ESCLAVE
! IMAST    /I/: NUMERO DE L'INTERFACE MAITRE. SI IMAST < 0, LES NOEUDS
!               COINCIDENT ET ABS(IMAST) EST L'INTERFACE MAITRE
! TRAMOD   /I-O/: TRACE DES MODES A PROJETER SUR L'INTERFACE MAITRE
!                 SUBSTITUE PAR LES MODES PROJETES EN SORTIE DE ROUTINE
!
!
!
#include "jeveux.h"
#include "asterc/matfpe.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/mgutdm.h"
#include "asterfort/rotati.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "blas/dgesvd.h"
!
!
!
!-- VARIABLES EN ENTREES / SORTIE
    integer :: ddlmas, ddlsla, nbmoma, nbmosl, imast
    character(len=8) :: nomres, matprj, sst1, sst2, intf1, intf2
    character(len=24) :: lino1, lino2, indin1, indin2
!
!-- VARIABLES DE LA ROUTINE
    integer :: ibid, i1, j1, k1, l1, lrot1, lrot2, ltran1, ltran2, nbno1, nbno2
    integer ::   lcoor1, lcoor2, numno, lno1, lno2, dima
    integer :: nbmast, nbslav, lnomas, lnosla,  indmin
    integer :: ltramo, lprojt, decal, lmats,  lmatv, lmsm1u
    integer :: lindma, lindsl, lwork,  possla, posmas, indsla
    integer(kind=4) :: info
    integer :: indmas
    character(len=8) :: kbid, mail1, mail2
    real(kind=8) :: tr1(3), tr2(3), ang1(3), ang2(3), rot1(3, 3), rot2(3, 3)
    real(kind=8) :: dismax, dismin, dx, dy, dz, swork(1)
    character(len=24) :: int1, int2, k24bid, tramod
    integer, pointer :: depend_noeuds(:) => null()
    real(kind=8), pointer :: dist_noeuds(:) => null()
    integer, pointer :: ind_int_mast(:) => null()
    integer, pointer :: ind_int_slav(:) => null()
    real(kind=8), pointer :: mat_phir(:) => null()
    real(kind=8), pointer :: mat_svd_work(:) => null()
    real(kind=8), pointer :: mat_u(:) => null()
    real(kind=8), pointer :: mat_vxsm1xut(:) => null()
    real(kind=8), pointer :: nlnoma1(:) => null()
    real(kind=8), pointer :: nlnoma2(:) => null()
!
!-----------C
!--       --C
!-- DEBUT --C
!--       --C
!-----------C
!
    call jemarq()
!
!-- RECUPERATION DES TRANSLATIONS ET ROTATIONS DES INTERFACES
    call jenonu(jexnom(nomres//'      .MODG.SSNO', sst1), ibid)
    call jeveuo(jexnum(nomres//'      .MODG.SSOR', ibid), 'L', lrot1)
    call jenonu(jexnom(nomres//'      .MODG.SSNO', sst1), ibid)
    call jeveuo(jexnum(nomres//'      .MODG.SSTR', ibid), 'L', ltran1)
!
    call jenonu(jexnom(nomres//'      .MODG.SSNO', sst2), ibid)
    call jeveuo(jexnum(nomres//'      .MODG.SSOR', ibid), 'L', lrot2)
!
    call jenonu(jexnom(nomres//'      .MODG.SSNO', sst2), ibid)
    call jeveuo(jexnum(nomres//'      .MODG.SSTR', ibid), 'L', ltran2)
!
    do i1 = 1, 3
        ang1(i1)=zr(lrot1+i1-1)
        ang2(i1)=zr(lrot2+i1-1)
        tr1(i1)=zr(ltran1+i1-1)
        tr2(i1)=zr(ltran2+i1-1)
    end do
!
    call rotati(ang1, rot1)
    call rotati(ang2, rot2)
!
!------------------------------------------------------------C
!--                                                        --C
!-- RECUPERATION DES COORDONNEES DES NOEUDS DE L'INTERFACE --C
!--                                                        --C
!------------------------------------------------------------C
!
!-- INTERFACE AMONT DE LA SOUS-STRUCTURE 1
    call mgutdm(nomres, sst1, ibid, 'NOM_LIST_INTERF', ibid,&
                kbid)
!-- NOEUDS DE LA SOUS STRUCTURE 1
    call mgutdm(nomres, sst1, ibid, 'NOM_MAILLAGE', ibid,&
                mail1)
    call jeveuo(mail1//'.COORDO    .VALE', 'L', vr=nlnoma1)
!-- NOMBRE DE NOEUDS DE L'INTERFACE 1
    int1=kbid//'.IDC_LINO'
    call jenonu(jexnom(int1(1:13)//'NOMS', intf1), ibid)
    call jelira(jexnum(int1, ibid), 'LONMAX', nbno1)
    call jeveuo(lino1, 'L', lno1)
!
!-- INTERFACE AMONT DE LA SOUS-STRUCTURE 2
    call mgutdm(nomres, sst2, ibid, 'NOM_LIST_INTERF', ibid,&
                kbid)
!-- NOMBRE DE NOEUDS DE L'INTERFACE 2
    int2=kbid//'.IDC_LINO'
    call jenonu(jexnom(int2(1:13)//'NOMS', intf2), ibid)
    call jelira(jexnum(int2, ibid), 'LONMAX', nbno2)
!-- NOEUDS DE LA SOUS STRUCTURE 2
    call mgutdm(nomres, sst2, ibid, 'NOM_MAILLAGE', ibid,&
                mail2)
    call jeveuo(mail2//'.COORDO    .VALE', 'L', vr=nlnoma2)
    call jeveuo(lino2, 'L', lno2)
!
!---------------------------------------------------------------C
!--                                                           --C
!-- TRANSFORMATION GEOM. DES COORD. DES NOEUDS DE L'INTERFACE --C
!--                                                           --C
!---------------------------------------------------------------C
!
!-- ALLOCATION DES VECTEURS DE COORDONNEES
    call wkvect('&&LIPSRB.COORD_INT1', 'V V R', 3*nbno1, lcoor1)
    call wkvect('&&LIPSRB.COORD_INT2', 'V V R', 3*nbno2, lcoor2)
!
!-- ROTATION ET TRANSLATION DES NOEUDS
    do i1 = 1, nbno1
        numno=zi(lno1+(i1-1))
        do j1 = 1, 3
            zr(lcoor1+(i1-1)*3+j1-1)=tr1(j1)+ rot1(j1,1)*nlnoma1(1+(&
            numno-1)*3)+ rot1(j1,2)*nlnoma1(1+(numno-1)*3+1)+ rot1(j1,&
            3)*nlnoma1(1+(numno-1)*3+2)
        end do
    end do
!
    do i1 = 1, nbno2
        numno=zi(lno2+(i1-1))
        do j1 = 1, 3
            zr(lcoor2+(i1-1)*3+j1-1)=tr2(j1)+ rot2(j1,1)*nlnoma2(1+(&
            numno-1)*3)+ rot2(j1,2)*nlnoma2(1+(numno-1)*3+1)+ rot2(j1,&
            3)*nlnoma2(1+(numno-1)*3+2)
        end do
    end do
!
!--------------------------------------------------------------------C
!--                                                                --C
!-- BOUCLE SUR LES NOEUDS ESCLAVES POUR TROUVER LES NOEUDS MAITRES --C
!--                                                                --C
!--------------------------------------------------------------------C
!
!-- CHOIX DU NOMBRE DE NOEUDS MAITRES
!
!-- PAR DEFAUT, ON EN PREND 5
    dima=5
!-- ON AJUSTE EN FONCTION DE LA DIMENSION DU MAILLAGE
    if (imast .eq. 1) then
        call dismoi('Z_CST', mail1, 'MAILLAGE', repk=k24bid)
        if (k24bid(1:3) .eq. 'OUI') dima=2
    else
        call dismoi('Z_CST', mail2, 'MAILLAGE', repk=k24bid)
        if (k24bid(1:3) .eq. 'OUI') dima=2
    endif
!-- ET DU NOMBRE DE NOEUDS MAITRES
    if (imast .eq. 1) then
        dima=min(dima,nbno1)
    else
        dima=min(dima,nbno2)
    endif
!
!-- IL FAUDRAIT TESTER QUE, EN 3D, POUR CHAQUE ESCLAVE, LES NOEUDS
!-- MAITRES NE SONT PAS ALIGNES.
!-- CF. LA CONSTRUCTION DE LA RELATION DE PROJECTION
!
!-- ON DUPLIQUE LES POINTEURS POUR MAITRES / ESCLAVES
    if (imast .le. 1) then
        nbmast=nbno1
        nbslav=nbno2
        lnomas=lcoor1
        lnosla=lcoor2
        call jeveuo(indin1, 'L', lindma)
        call jeveuo(indin2, 'L', lindsl)
    else
        nbmast=nbno2
        nbslav=nbno1
        lnomas=lcoor2
        lnosla=lcoor1
        call jeveuo(indin2, 'L', lindma)
        call jeveuo(indin1, 'L', lindsl)
    endif
!
!-- ALLOCATION DE LA MATRICE DE DEPENDANCES
    AS_ALLOCATE(vi=depend_noeuds, size=nbslav*dima)
!-- ALLOCATION DE LA MATRICE DES DISTANCES
    AS_ALLOCATE(vr=dist_noeuds, size=nbmast)
!
    do i1 = 1, nbslav
!-- DISTANCE AU NOEUD ESCLAVE COURANT
        dismax=0.d0
        dismin=1.d16
!
        do j1 = 1, nbmast
            dist_noeuds(j1)= (zr(lnosla+(i1-1)*3)- zr(lnomas+(j1-1)*3))&
            **2+ (zr(lnosla+(i1-1)*3+1)- zr(lnomas+(j1-1)*3+1))**2+&
            (zr(lnosla+(i1-1)*3+2)- zr(lnomas+(j1-1)*3+2))**2
            if (dist_noeuds(j1) .gt. dismax) then
                dismax=dist_noeuds(j1)
            endif
            if (dist_noeuds(j1) .lt. dismin) then
                dismin=dist_noeuds(j1)
                indmin=j1
            endif
        end do
!
!-- RECHERCHE DES DIMA PLUS PROCHES VOISINS
        if (dima .eq. 1) then
            depend_noeuds(i1)=indmin
        else
            depend_noeuds(1+(i1-1)*dima)=indmin
            do j1 = 1, dima-1
                dist_noeuds(indmin)=dismax
                dismin=dismax
                do k1 = 1, nbmast
                    if (dist_noeuds(k1) .lt. dismin) then
                        dismin=dist_noeuds(k1)
                        indmin=k1
                    endif
                end do
                depend_noeuds(1+(i1-1)*dima+j1)=indmin
            end do
        endif
    end do
!
!---------------------------------------------C
!--                                         --C
!-- CONSTRUCTION DES RELATIONS CINEMATIQUES --C
!--                                         --C
!---------------------------------------------C
!--
!-- MATRICE_PHIR CORRESPOND A LA MATRICE D'UN MOUVEMENT DE
!-- CORPS RIGIDE LINEARISE AUTOUR DU NOEUD ESCLAVE
!--
!-- DEP CORRESPOND AUX 6 COMPOSANTES DE DEPLACEMENT / ROTATION
!--         _                       _  _    _
!--        |  1   0   0   0   0   0 | |  DX  |
!--        |  0   1   0   0   0   0 | |  DY  |
!--        |  0   0   1   0   0   0 | |  DZ  |
!-- Q_MAST=|  0  -LZ  LY  1   0   0 | |  DRX | = PHI_RIGI.AMP_GEN_RIGI
!--        |  LZ  0  -LX  0   1   0 | |  DRY |
!--        |_-LY  LX  0   0   0   1_| |_ DRZ_|
!--
!--   OU DX, DY, DZ, DRX, DRY ET DRZ SONT LES AMPLITUDES DU MODE,
!--   ET LX, LY ET LZ LA DISTANCE ENTRE LE NOEUD COURANT ET LE NOEUD
!--   ESCLAVE DANS LES DIRECTIONS X, Y ET Z
!--
!-- ON CONSTRUIT CES MATRICES POUR CHAQUE NOEUD MAITRE, ON LES CONCATENE
!-- ET ON CALCULE LA PSEUDO INVERSE QUI PERMET DE PROJETER LES
!-- MOUVEMENTS D'UN ENSEMBLE DE NOEUD MAITRE SUR LE MOUVEMENT DE CORPS
!-- RIGIDE MOYEN DU NUAGE DE NOEUDS MAITRES, DE SORTE QUE LE MOUVEMENT
!-- DES ESCLAVES S'ECRIVE :
!--
!--     Q_SLAV = (PHI_RIGIDE)^+ Q_MAST
!--
!-- OU ^+  CORRESPOND A LA PSEUDO INVERSE
!
!-- ALLOCATION DE LA MATRICE PROJETEE
    call wkvect('&&LIPSRB.TR_MOD_MAST_PRO', 'V V R', ddlsla*nbmoma, lprojt)
!
!-- ALLOCATION DES MATRICES DE TRAVAIL TEMPORAIRES
    AS_ALLOCATE(vr=mat_phir, size=dima*36)
!
    call wkvect('&&LIPSRB.MAT_S', 'V V R', 6, lmats)
    AS_ALLOCATE(vr=mat_u, size=36)
    call wkvect('&&LIPSRB.MAT_V', 'V V R', 36*dima*dima, lmatv)
!
!-- DESACTIVATION DU TEST FPE
    call matfpe(-1)
!
    call dgesvd('A', 'A', 6, 6*dima, mat_phir,&
                6, zr(lmats), mat_u, 6, zr(lmatv),&
                6*dima, swork, -1, info)
    lwork=int(swork(1))
    AS_ALLOCATE(vr=mat_svd_work, size=lwork)
!
    call wkvect('&&LIPSRB.MAT_SM1XUT', 'V V R', 36, lmsm1u)
    AS_ALLOCATE(vr=mat_vxsm1xut, size=36*dima)
!
!-- CONSTRUCTION DES VECTEURS D'INDICES POUR LES DDL
!--     EN RENUMEROTANT L'INTERFACE DE 1 A DDLMAS/DDLSLA
    possla=1
    posmas=1
    AS_ALLOCATE(vi=ind_int_mast, size=6*nbmast)
    AS_ALLOCATE(vi=ind_int_slav, size=6*nbslav)
    do i1 = 1, 6*nbmast
        if (zi(lindma+i1-1) .gt. 0) then
            ind_int_mast(i1)=posmas
            posmas=posmas+1
        endif
    end do
    do i1 = 1, 6*nbslav
        if (zi(lindsl+i1-1) .gt. 0) then
            ind_int_slav(i1)=possla
            possla=possla+1
        endif
    end do
!
!------------------------------C
!--                          --C
!-- PROJECTION DE LA MATRICE --C
!--                          --C
!------------------------------C
    call jeveuo(tramod, 'L', ltramo)
!
    do i1 = 1, nbslav
!-- INITIALISER A CHAQUE FOIS, PUISQUE LA SVD ECRASE TOUT...
        do j1 = 1, dima*36
            mat_phir(j1)=0.d0
        end do
!
!-- CONSTRUCTION DE LA MATRICE DE CORPS RIGIDE POUR LE NOEUD COURANT
        do j1 = 1, dima
            numno=depend_noeuds(1+(i1-1)*dima+j1-1)-1
            dx=(zr(lnomas+numno*3)- zr(lnosla+(i1-1)*3))
            dy=(zr(lnomas+numno*3+1)- zr(lnosla+(i1-1)*3+1))
            dz=(zr(lnomas+numno*3+2)- zr(lnosla+(i1-1)*3+2))
            decal=(j1-1)*36
            do k1 = 1, 6
!
!-- POUR RECONSTRUIRE LES ROTATIONS, ON GARDE TOUTES LES COMPOSANTES
!-- SI LES NOEUDS DE L'INTERFACE MAITRE PORTENT LES DDL DRX DRY ET DRZ
!-- (CAS 2D), ET UNIQUEMENT LES TRANSLATIONS SINON (CAS 3D)
!
!            ZR(LPHIR+DECAL+(K1-1)*7)=1.
!            ZR(LPHIR+DECAL+(K1-1)*7)=1.-INT((K1-1)/3)
                if (zi(lindma+6*numno+k1-1) .gt. 0) then
                    mat_phir(1+decal+(k1-1)*7)=1.d0
                else
                    mat_phir(1+decal+(k1-1)*7)=0.d0
                endif
!
            end do
            mat_phir(1+decal+4)= dz
            mat_phir(1+decal+5)= -dy
            mat_phir(1+decal+9)= -dz
            mat_phir(1+decal+11)= dx
            mat_phir(1+decal+15)= dy
            mat_phir(1+decal+16)=-dx
        end do
!
!-- CONSTRUCTION DE LA MATRICE D'OBSERVATION
!
        call dgesvd('A', 'A', 6, 6*dima, mat_phir,&
                    6, zr(lmats), mat_u, 6, zr(lmatv),&
                    6*dima, mat_svd_work, lwork, info)
!
!-- VOIR A RAJOUTER UN TEST EN FONCTION DE LA DIMENSION DU MAILLAGE,
!-- POUR LE RECOLLEMENT DES INTERFACES
        do k1 = 1, 6
            do j1 = 1, 6
!-- TEST SUR LA VALEUR SINGULIERE, POUR LIMITER LES PB DE
!-- CONDITIONNEMENT
                if (abs(zr(lmats+j1-1)) .gt. 1.d-10) then
                    zr(lmsm1u+(k1-1)*6+j1-1)=(1/zr(lmats+j1-1))*&
                    mat_u(1+(j1-1)*6+k1-1)
                endif
            end do
        end do
!
        do k1 = 1, 6
            do j1 = 1, 6*dima
                mat_vxsm1xut(1+(k1-1)*6*dima+j1-1)=0.d0
                do l1 = 1, 6
                    mat_vxsm1xut(1+(k1-1)*6*dima+j1-1)= mat_vxsm1xut(1+(k1-1)*6*&
                    dima+j1-1)+ zr(lmatv+(j1-1)*6*dima+l1-1)* zr(&
                    lmsm1u+(k1-1)*6+l1-1)
                end do
            end do
        end do
!
!-- REMPLISSAGE DE LA MATRICE PROJETEE
!-- ON VERIFIE A CHAQUE PASSAGE QUE LES DDL MAITRES
!-- ET ESCLAVES EXISTENT DANS LA DEFINITION DES INTERFACES
!
        do j1 = 1, 6
            indsla=ind_int_slav(1+(i1-1)*6+j1-1)
            if (indsla .gt. 0) then
                do l1 = 1, nbmoma
                    zr(lprojt+(l1-1)*ddlsla+indsla-1)=0.d0
                    do k1 = 1, 6*dima
                        numno=depend_noeuds(1 +(i1-1)*dima+int((k1-1)/6))
                        indmas=ind_int_mast(1+(numno-1)*6+mod(k1-1,6))
                        if (indmas .gt. 0) then
                            zr(lprojt+(l1-1)*ddlsla+indsla-1)=&
                            zr(lprojt+(l1-1)*ddlsla+indsla-1)+&
                            mat_vxsm1xut(1+(j1-1)*6*dima+k1-1)* zr(ltramo+(&
                            l1-1)*ddlmas+indmas-1)
                        endif
                    end do
                end do
            endif
        end do
    end do
!
!-- REACTIVATION DU TEST FPE
    call matfpe(1)
!
!-- SUBSTITUTION DES MODES A PROJETER PAR LES MODES PROJETES
    call jedetr(tramod)
    tramod='&&LIPSRB.TR_MOD_MAST_PRO'
!
!-- DESTRUCTION DES MATRICES TEMPORAIRES
!
    AS_DEALLOCATE(vi=ind_int_mast)
    AS_DEALLOCATE(vi=ind_int_slav)
    AS_DEALLOCATE(vr=mat_phir)
    call jedetr('&&LIPSRB.MAT_TRACE_PROJ')
    call jedetr('&&LIPSRB.MAT_S')
    AS_DEALLOCATE(vr=mat_u)
    call jedetr('&&LIPSRB.MAT_V')
    call jedetr('&&LIPSRB.MAT_SM1XUT')
    AS_DEALLOCATE(vr=mat_vxsm1xut)
    AS_DEALLOCATE(vr=mat_svd_work)
    call jedetr('&&LIPSRB.COORD_INT1')
    call jedetr('&&LIPSRB.COORD_INT2')
    AS_DEALLOCATE(vi=depend_noeuds)
    AS_DEALLOCATE(vr=dist_noeuds)
!
!---------C
!--     --C
!-- FIN --C
!--     --C
!---------C
!
    call jedema()
end subroutine
