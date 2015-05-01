subroutine phi152(model, option, mate, phibar, ma,&
                  nu, num, nbmode, solvez, indice,&
                  tabad)
    implicit none
!---------------------------------------------------------------------
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
!---------------------------------------------------------------------
! AUTEUR : G.ROUSSEAU
!
! CALCULS DES CONDITIONS AUX LIMITES POUR LA DETERMINATION
! DES POTENTIELS FLUCTUANTS POUR LA MASSE AJOUTEE, LA RAIDEUR
! AJOUTEE ET L AMORTISSEMENT AJOUTE EN THEORIE POTENTIELLE
! ET RESOLUTION DES PROBLEMES DE LAPLACE ASSOCIES
! IN : K* : MODEL : TYPE DE MODELISATION FLUIDE
! IN : K* : OPTION : OPTION DE CALCUL DES GRANDEURS AJOUTEES
! IN : K* : MATE : MATERIAU FLUIDE
! IN : K* : PHIBAR : NOM DU POTENTIEL PERMANENT
! IN : K* : MA : NOM DE LA MATRICE DE RAIDEUR FLUIDE
! IN : K* : NU : NUMEROTATION DES DDLS ASSOCIES AU FLUIDE
! IN : K* : NUM : NUMEROTATION DES DDLS ASSOCIES A L'INTERFACE
!           POTENTIELS FLUCTUANTS : 1 : MASSE AJOUTEE
!                                 : 2 : AMORTISSEMENT ET RAIDEUR
! IN : K* : SOLVEZ : METHODE DE RESOLUTION 'MULT_FRONT','LDLT' OU 'GCPC'
!---------------------------------------------------------------------
#include "jeveux.h"
#include "asterfort/cal2m.h"
#include "asterfort/calflu.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupc.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/majou.h"
#include "asterfort/prstoc.h"
#include "asterfort/resoud.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsorac.h"
#include "asterfort/tabcor.h"
#include "asterfort/wkvect.h"
    integer :: ibid, nbvale, nbrefe, nbdesc, nbmode, iret
    integer :: ilires, j, nbid, ivalk, indice, tabad(5), tmod(1)
    integer :: iphi1, iphi2, n5, n6, n7, n1, icor(2), n2, ndble
    real(kind=8) :: bid, ebid
    character(len=*) :: option, mate, phibar, solvez
    character(len=2) :: model
    character(len=8) :: k8bid, modmec, mailla, maflui, ma
    character(len=8) :: moflui, moint
    character(len=14) :: nu, num
    character(len=19) :: vecso1, vecso2, maprec, solveu, chsol
    character(len=19) :: vesto1, vesto2, chamno
    character(len=24) :: nomcha
    character(len=24) :: phib24, criter
    complex(kind=8) :: cbid
    data maprec   /'&&OP0152.MAPREC'/
    data chsol    /'&&OP0152.SOLUTION'/
! -----------------------------------------------------------------
    data ndble /0/
!
    call jemarq()
    solveu = solvez
    criter = '&&RESGRA_GCPC'
    indice=0
    call getvid(' ', 'MODE_MECA', scal=modmec, nbret=n5)
    call getvid(' ', 'MODELE_FLUIDE', scal=moflui, nbret=n1)
    call getvid(' ', 'MODELE_INTERFACE', scal=moint, nbret=n2)
    call getvid(' ', 'CHAM_NO', nbval=0, nbret=n6)
!
! TEST POUR DETERMINER SI FLUIDE ET STRUCTURE S APPUIENT SUR
! DES MAILLAGES COMMUNS
    if (n5 .gt. 0) then
        call rsexch(' ', modmec, 'DEPL', 1, nomcha,&
                    iret)
        call rsorac(modmec, 'LONUTI', ibid, bid, k8bid,&
                    cbid, ebid, 'ABSOLU', tmod, 1,&
                    nbid)
        nbmode=tmod(1)
        call dismoi('NOM_MAILLA', nomcha(1:19), 'CHAM_NO', repk=mailla)
        call dismoi('NOM_MAILLA', moint, 'MODELE', repk=maflui)
        if (maflui .ne. mailla) then
            call tabcor(model, mate, mailla, maflui, moint,&
                        num, ndble, icor)
            call majou(model, modmec, solveu, num, nu,&
                       ma, mate, moint, ndble, icor,&
                       tabad)
            indice=1
        endif
    endif
!
!---------------------------------------------------------------------
    if (n6 .ne. 0) then
        n7 = -n6
    else
        n7=0
    endif
!
!
!=====================================================================
!---------------- ALTERNATIVE CHAMNO OU MODE_MECA OU---------
!-----------------------------MODELE-GENE--------------------
!=====================================================================
! DANS LE CAS OU ON N A PAS CALCUL DE MASSE AJOUTEE SUR UN
! MAILLAGE SQUELETTE
!
    if ((n5.gt.0) .and. (indice.ne.1)) then
!
!
!----- -RECUPERATION DU NB DE MODES DU CONCEPT MODE_MECA
!
        call rsorac(modmec, 'LONUTI', ibid, bid, k8bid,&
                    cbid, ebid, 'ABSOLU', tmod, 1,&
                    nbid)
        nbmode=tmod(1)
!
        call wkvect('&&OP0152.PHI1', 'V V K24', nbmode, iphi1)
        call wkvect('&&OP0152.PHI2', 'V V K24', nbmode, iphi2)
!
!======================================================================
! BOUCLE SUR LE NOMBRE DE MODES: CALCUL DU FLUX FLUIDE MODAL
!======================================================================
        ilires = 0
        phib24=phibar
!
        do j = 1, nbmode
!
            call rsexch(' ', modmec, 'DEPL', j, nomcha,&
                        iret)
!
            nomcha=nomcha(1:19)
            vecso1 = '&&OP0152.VECSOL1'
            vecso2 = '&&OP0152.VECSOL2'
!
            call calflu(nomcha, moflui, mate, nu, vecso1,&
                        nbdesc, nbrefe, nbvale, 'R')
!
            ilires = ilires + 1
!
!------------- RESOLUTION  DU LAPLACIEN EN 2D-----------------------
!
            call resoud(ma, maprec, solveu, ' ', 0,&
                        vecso1, chsol, 'V', [0.d0], [cbid],&
                        criter, .true._1, 0, iret)
            call jedupc('V', chsol(1:19), 1, 'V', vecso1(1:19),&
                        .false._1)
            call detrsd('CHAMP_GD', chsol)
!
!------------ CREATION DU VECTEUR PRESSION MODAL-------------------
!
!- FORMATION DU TABLEAU CONTENANT LA PRESSION POUR CHAQUE MODE-----
!
!------------------------------------------------------------------
            vesto1='&&OP0152.VEST1'
            call prstoc(vecso1, vesto1, ilires, ilires, iphi1,&
                        nbvale, nbrefe, nbdesc)
!
            if (option .eq. 'AMOR_AJOU' .or. option .eq. 'RIGI_AJOU') then
!
                call cal2m(nomcha(1:19), phib24, moflui, mate, nu,&
                           vecso2, nbdesc, nbrefe, nbvale)
!
                call resoud(ma, maprec, solveu, ' ', 0,&
                            vecso2, chsol, 'V', [0.d0], [cbid],&
                            criter, .true._1, 0, iret)
                call jedupc('V', chsol(1:19), 1, 'V', vecso2(1:19),&
                            .false._1)
                call detrsd('CHAMP_GD', chsol)
!
                vesto2='&&OP0152.VEST2'
                call prstoc(vecso2, vesto2, ilires, ilires, iphi2,&
                            nbvale, nbrefe, nbdesc)
!
            endif
!
        end do
!
    else
        if ((n7.gt.0) .and. (indice.ne.1)) then
!
!================================================================
! ON FAIT LA MEME OPERATION SUR LES CHAMNO DE DEPL_R FOURNIS
!================================================================
!
            call wkvect('&&OP0152.PHI1', 'V V K24', n7, iphi1)
            call wkvect('&&OP0152.PHI2', 'V V K24', n7, iphi2)
            call wkvect('&&OP0152.VEC', 'V V K8', n7, ivalk)
            call getvid(' ', 'CHAM_NO', nbval=n7, vect=zk8(ivalk), nbret=n6)
!
            ilires = 0
            phib24=phibar
!
            do j = 1, n7
!
                chamno=zk8(ivalk+j-1)
                vecso1 = '&&OP0152.VESL1'
                vecso2 = '&&OP0152.VESL2'
!
                call calflu(chamno, moflui, mate, nu, vecso1,&
                            nbdesc, nbrefe, nbvale, 'R')
!
                ilires = ilires + 1
!
!
!-------------- RESOLUTION  DU LAPLACIEN EN 2D OU 3D-------------
!
                call resoud(ma, maprec, solveu, ' ', 0,&
                            vecso1, chsol, 'V', [0.d0], [cbid],&
                            criter, .true._1, 0, iret)
                call jedupc('V', chsol(1:19), 1, 'V', vecso1(1:19),&
                            .false._1)
                call detrsd('CHAMP_GD', chsol)
!
!--------------- CREATION DU VECTEUR PRESSION -------------------
!
!--------- FORMATION DU TABLEAU CONTENANT LA PRESSION------------
!-------------POUR CHAQUE CHAMP AUX NOEUDS-----------------------
!
                vesto1='&&OP0152.VEST1'
                call prstoc(vecso1, vesto1, ilires, ilires, iphi1,&
                            nbvale, nbrefe, nbdesc)
!
                if (option .eq. 'AMOR_AJOU' .or. option .eq. 'RIGI_AJOU') then
                    call cal2m(chamno, phib24, moflui, mate, nu,&
                               vecso2, nbdesc, nbrefe, nbvale)
                    call resoud(ma, maprec, solveu, ' ', 0,&
                                vecso2, chsol, 'V', [0.d0], [cbid],&
                                criter, .true._1, 0, iret)
                    call jedupc('V', chsol(1:19), 1, 'V', vecso2(1:19),&
                                .false._1)
                    call detrsd('CHAMP_GD', chsol)
!
                    vesto2='&&OP0152.VEST2'
                    call prstoc(vecso2, vesto2, ilires, ilires, iphi2,&
                                nbvale, nbrefe, nbdesc)
                endif
!
            end do
        endif
    endif
!
    call jeexin(criter(1:19)//'.CRTI', iret)
    if (iret .ne. 0) then
        call jedetr(criter(1:19)//'.CRTI')
        call jedetr(criter(1:19)//'.CRTR')
        call jedetr(criter(1:19)//'.CRDE')
    endif
!
!----------------------------------------------------------------
    call jedema()
end subroutine
