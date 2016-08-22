subroutine statim(nbobst, nbpt, temps, fcho, vgli,&
                  vint, wk1, wk2, wk3, tdebut,&
                  tfin, nbloc, offset, trepos, nbclas,&
                  noecho, intitu, nomres, nbvint)
    implicit none
#include "jeveux.h"
#include "asterfort/histog.h"
#include "asterfort/impact.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/mdtr74grd.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/utmess.h"
#include "blas/dcopy.h"
    integer :: nbobst, nbpt, nbloc, nbvint
    real(kind=8) :: temps(*), fcho(*), vgli(*), tdebut, tfin
    real(kind=8) :: wk1(*), wk2(*), wk3(*), fnmaxa, fnmety, fnmmoy
    real(kind=8) :: offset, trepos, vint(*)
    character(len=8) :: noecho(*), intitu(*)
    character(len=*) :: nomres
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
!     CALCUL ET IMPRESSION DES STATISTIQUES DE CHOC
!
!     NBOBST       : NB DE NOEUDS DE CHOC
!     NBPT         : NB DE PAS DE TEMPS TEMPORELS ARCHIVES
!     NBLOC        : NB DE BLOCS POUR LE MOYENNAGE
!     TEMPS        : INSTANTS DE CALCUL
!     FCHO         : VECTEUR DES FORCES DE CHOC
!     VGLI         : VECTEUR DES VITESSES DE GLISSEMENT
!     VINT         : Variables internes
!     NBCLAS       : NOMBRE DE CLASSES
!
!-----------------------------------------------------------------------
    integer :: ibid, nbpara, nparg, nparp, nparf
!-----------------------------------------------------------------------
    integer :: i, idebut, idec, ifin, ipas, nbchoc, nbclas
    integer :: nbpas, ndec, npari
    real(kind=8) :: dt, fmax, fmin
!-----------------------------------------------------------------------
    parameter    ( nparg = 6 , nparp = 7 , npari = 10 , nbpara = 20 )
    parameter    ( nparf = 7 )
    real(kind=8) :: para(3),forcefl
    complex(kind=8) :: c16b
    character(len=4) :: tpara(nbpara)
    character(len=8) :: noeud
    character(len=16) :: tvar(4), lpari(npari), lparg(nparg), lparp(nparp)
    character(len=16) :: valek(4), npara(nbpara), lparf(nparf)
!
    data tvar  / 'IMPACT' , 'GLOBAL' , 'PROBA'  , 'FLAMBAGE' /
!
    data npara/ 'INTITULE','NOEUD', 'CALCUL'       , 'CHOC'         ,&
                'INSTANT'       , 'F_MAX'         , 'IMPULSION'    ,&
                'T_CHOC'        , 'V_IMPACT'      , 'NB_IMPACT'    ,&
                'F_MAX_ABS'     , 'F_MAX_MOY'     , 'F_MAX_ETYPE'  ,&
                'CLASSE'        , 'DEBUT'         , 'FIN'          ,&
                'PROBA'         , 'FLAMBAGE'      , 'ECRAS_RESI'   ,&
                'INST_FLAMB'    /
    data tpara/ 'K8', 'K8'      , 'K16'           , 'I'            ,&
                'R'             , 'R'             , 'R'            ,&
                'R'             , 'R'             , 'I'            ,&
                'R'             , 'R'             , 'R'            ,&
                'I'             , 'R'             , 'R'            ,&
                'R'             , 'K8'            , 'R'            ,&
                'R'             /
!
    data lpari/ 'INTITULE','NOEUD', 'CALCUL'      , 'CHOC'         ,&
                'INSTANT'       , 'F_MAX'         , 'IMPULSION'    ,&
                'T_CHOC'        , 'V_IMPACT'      , 'NB_IMPACT'    /
!
    data lparg/ 'INTITULE'      ,'NOEUD'          ,'CALCUL'      ,&
                'F_MAX_ABS'     , 'F_MAX_MOY'     , 'F_MAX_ETYPE'  /
!
    data lparp/ 'INTITULE','NOEUD','CALCUL'       , 'CLASSE'        ,&
                'DEBUT'         , 'FIN'           , 'PROBA'         /
    data lparf/ 'INTITULE','NOEUD','CALCUL'       , 'CHOC'         ,&
                'FLAMBAGE'      , 'ECRAS_RESI'    , 'INST_FLAMB'    /
!-----------------------------------------------------------------------
!
    call jemarq()
    c16b=(0.d0,0.d0)
    ibid=0
!
    dt = (temps(nbpt)-temps(1))/(nbpt-1)
    idebut = int((tdebut-temps(1))/dt)+1
    ifin = min(int((tfin-temps(1))/dt)+1,nbpt)
    nbpas = ifin - idebut + 1
!
    if (nbloc .eq. 0) nbloc = 1
    if (nbloc .gt. 1) then
        call utmess('I', 'ALGORITH10_76')
        nbloc = 1
    endif
    if (nbclas .eq. 0) nbclas = 10
!
    call tbcrsd(nomres, 'G')
    call tbajpa(nomres, nbpara, npara, tpara)
!
!   Boucle sur les noeuds de choc
    do i = 1, nbobst
        noeud = noecho(i)
        valek(1) = intitu(i)
        valek(2) = noeud
        valek(3) = tvar(1)
!
        nbchoc = 0
        call dcopy(nbpas, fcho(3*(i-1)+1), 3*nbobst, wk1, 1)
        call dcopy(nbpas, vgli(3*(i-1)+1), 3*nbobst, wk2, 1)
        call impact(nomres, nbpas, wk1(idebut), wk2(idebut), wk3,&
                    offset, temps(idebut), trepos, nbchoc, fnmaxa,&
                    fnmmoy, fnmety, npari, lpari, valek)
!
        valek(3) = tvar(2)
        para(1) = fnmaxa
        para(2) = fnmmoy
        para(3) = fnmety
        call tbajli(nomres, nparg, lparg, [ibid], para,&
                    [c16b], valek, 0)
!
        ndec = nbclas
        fmin = 1.d50
        fmax = -fmin
        call histog(nbchoc, wk3, fmin, fmax, wk2,&
                    wk1, ndec)
!
        valek(3) = tvar(3)
        do idec = 1, ndec
            if (idec .eq. 1) then
                para(1) = fmin
            else
                para(1) = wk2(idec-1)
            endif
            para(2) = wk2(idec)
            para(3) = wk1(idec)
            call tbajli(nomres, nparp, lparp, [idec], para,&
                        [c16b], valek, 0)
        enddo
!
!       AJOUT FLAMBAGE SI CELUI-CI A EU LIEU
!       Si au dernier pas de temps le flambage a eu lieu on cherche l'instant correspondant
        forcefl = vint((nbpas-1)*nbvint+i)
        if (forcefl .gt. 0.d0) then
            valek(3) = tvar(4)
            valek(4) = 'OUI'
            para(1) = forcefl
            ipas = 1
!           Quand forcefl devient >0 ==> on a trouvé l'instant
40          continue
            forcefl = vint((ipas-1)*nbvint+i)
            if ( forcefl .gt. 0.0d0 ) then
                para(2) = temps(ipas)
            else
                ipas = ipas + 1
                if (ipas .le. nbpas) goto 40
            endif
            if (noecho(nbobst+i).ne.' ') then
!               cas choc entre 2 noeuds : on repartit l'effort
                para(1) = para(1)/2.d0
!               1er noeud
                call tbajli(nomres, nparf, lparf, [i], para,&
                            [c16b], valek, 0)
!               2eme noeud
                ! valek(1) = intitu(nbobst+i)
                valek(2) = noecho(nbobst+i)
                call tbajli(nomres, nparf, lparf, [i], para,&
                            [c16b], valek, 0)
            else
                call tbajli(nomres, nparf, lparf, [i], para,&
                            [c16b], valek, 0)
            endif
        endif
    enddo
!
    call jedema()
end subroutine
