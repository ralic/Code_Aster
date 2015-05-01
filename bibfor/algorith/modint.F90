subroutine modint(ssami, raiint, nddlin, nbmod, shift,&
                  matmod, masse, raide, neq, coint,&
                  noddli, nnoint, vefreq, switch)
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
!    M. CORUS     DATE 05/02/10
!-----------------------------------------------------------------------
!
!  BUT:      < CALCUL DES MODES D'INTERFACE >
!
!  ON EXTRAIT, A PARTIR DE LA SOUS MATRICE ASSOCIEE A L'INTERFACE, LA
!  CONNECTIVITE DU TREILLIS DE POUTRE SOUS JACENT. ON DETERMINE LE
!  NOMBRE DE PARTIES INDEPENDANTES DE L'INTERFACE, ET ON CALCULE, POUR
!  CHAQUE PARTIE, LES PREMIERS MODES PROPRES, EN PRENANT SOIN DE BIEN
!  CAPTER LES MODES DE CORPS RIGIDE. ON CONSTRUIT ENSUITE, SUR LA BASE
!  DE CES MODES, UN SOUS ESPACE POUR PROJETER LES MATRICES DU PROBLEME
!  COMPLET, ET ON CALCULE, SUR CE SOUS ESPACE, LES MODES D'INTERFACE.
!
!-----------------------------------------------------------------------
!  IN  : SSAMI   : MATRICE DE MASSE DU MODELE D'INTERFACE
!  IN  : RAIINT  : MATRICE DE RAIDEUR DU MODELE D'INTERFACE
!  IN  : NDDLIN  : NOMBRE D'EQUATIONS DU NUME_DDL D'INTERFACE
!  IN  : NBMOD   : NOMBRE DE MODES D'INTERFACE DEMANDE
!  IN  : SHIFT   : VALEUR DE FREQUENCE POUR LE DECALAGE DE LA RAIDEUR
!  OUT : MATMOD  : MATRICE DES MODES D'INTERFACE
!  IN  : MASSE   : MATRICE DE MASSE DU MODELE COMPLET
!  IN  : RAIDE   : MATRICE DE RAIDEUR DU MODELE COMPLET
!  IN  : NEQ     : NOMBRE D'EQUATIONS DU NUME_DDL COMPLET
!  IN  : COINT   : DEFINITION DE LA CONNECTIVITE DE L'INTERFACE
!  IN  : NODDLI  : DEFINITION DES DDL PORTES PAR LES NOEUDS D'INTERFACE
!  IN  : NNOINT  : NOMBRE DE NOEUD A L'INTERFACE
!  OUT : VEFREQ  : NOM DU VECTEUR CONTENANT LES FREQUENCES PROPRES
!  IN  : SWITCH  : 1 SI ON DOIT RELEVER LES MODES SUR TOUTE LA SST
!                  0 SI ON NE CONSERVE QUE LES MODES SUR L'INTERFACE
!
!-----------------------------------------------------------------------
!
!
!
!
!     ------------------------------------------------------------------
!
!-- VARIABLES EN ENTREES / SORTIE
#include "jeveux.h"
#include "asterc/getran.h"
#include "asterc/matfpe.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvr8.h"
#include "asterfort/intdis.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetc.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/infniv.h"
#include "asterfort/nmop45.h"
#include "asterfort/mrmult.h"
#include "asterfort/mtcmbl.h"
#include "asterfort/mtdefs.h"
#include "asterfort/mtdscr.h"
#include "asterfort/preres.h"
#include "asterfort/resoud.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "blas/ddot.h"
#include "blas/dgeev.h"
#include "blas/dggev.h"
    integer :: nddlin, nbmod, nnoint, neq, switch
    real(kind=8) :: shift
    character(len=19) :: masse, raide, ssami, raiint
    character(len=24) :: coint, noddli, matmod, vefreq
    character(len=8)  :: modes
    character(len=6)  :: k6bid
    character(len=24) :: dummy1,dummy3
    character(len=16) :: nmopt
    
    
!
!-- VARIABLES DE LA ROUTINE
    integer :: lmatmo, i1, j1, k1, m1, lmakry, nsekry
    integer ::       lmatk, lmatm, lmapro
    integer :: lkpro,  lmatrm, lmatrk, lwork
    integer ::   limped,   lmatma, iret
    integer :: nbvect, ibid,   no, nbsst, lindin, coeff, lvp
    integer :: ifm,niv
    integer(kind=4) :: info
    real(kind=8) :: temp, pi, rbid, norm, lambda, comlin(2), swork(1), max
    real(kind=8) :: abs, bande(2)
    parameter    (pi=3.141592653589793238462643d0)
    complex(kind=8) :: cbid
    character(len=1) :: listyp(2)
    character(len=19) :: lismat(2), imped, solveu, nume91, nume, prno
    character(len=24) :: valk
    real(kind=8), pointer :: hessenberg(:) => null()
    real(kind=8), pointer :: imagpart(:) => null()
    real(kind=8), pointer :: leftmodes(:) => null()
    real(kind=8), pointer :: matr_eigen_work(:) => null()
    real(kind=8), pointer :: matr_mod_red(:) => null()
    real(kind=8), pointer :: matr_work_dggev(:) => null()
    real(kind=8), pointer :: realpart(:) => null()
    real(kind=8), pointer :: rightmodes(:) => null()
    real(kind=8), pointer :: vect_alphai(:) => null()
    real(kind=8), pointer :: vect_alphar(:) => null()
    real(kind=8), pointer :: vect_beta(:) => null()
    real(kind=8), pointer :: vect_temp_2(:) => null()
    real(kind=8), pointer :: v_f_pro(:) => null()
    integer, pointer :: v_ind_f_pro(:) => null()
    real(kind=8), pointer :: vale(:) => null()
    integer, pointer :: v_ind_lag(:) => null()
    integer, pointer :: delg(:) => null()
    integer, pointer :: ddl_actif_int(:) => null()
    cbid = dcmplx(0.d0, 0.d0)
!
!-- DEBUT --C
!
    call jemarq()
    call infniv(ifm, niv)    
!
!------------------------------------------------------------C
!--                                                        --C
!-- CONSTRUCTION DES MATRICES D'IMPEDANCE DYNAMIQUE K+MU*M --C
!--            ET DE MASSE DU MODELE D'INTERFACE           --C
!--                                                        --C
!------------------------------------------------------------C
!
!-- ON DESACTIVE LE TEST FPE
    call matfpe(-1)
    
    call mtdscr(ssami)
    call jeveuo(ssami(1:19)//'.&INT', 'L', lmatma)
!
    imped='&&MOIN93.RAID_SHIFT'
    call mtdefs(imped, raiint, 'V', ' ')
    lismat(1)=raiint
    lismat(2)=ssami
    if (switch .eq. 1) then
        call getvr8('MODE_INTERF', 'SHIFT', iocc=1, scal=rbid, nbret=ibid)
        shift=-(rbid*2.d0*pi)**2  
    endif
!
    comlin(1)=1.d0
    comlin(2)=shift
    listyp(1)='R'
    listyp(2)='R'
!
    call mtcmbl(2, listyp, comlin, lismat, imped,&
                ' ', nume91, 'ELIM1')
    call mtdscr(imped)
    call jeveuo(imped(1:19)//'.&INT', 'E', limped)
    call dismoi('SOLVEUR', ssami, 'MATR_ASSE', repk=solveu)
    call dismoi('NOM_NUME_DDL', ssami, 'MATR_ASSE', repk=nume91)
    call preres(solveu, 'V', iret, '&&OP0091.MATPRE', imped,&
                ibid, 1)
!
    if (iret .eq. 2) then
        valk = imped
        call utmess('F', 'ALGELINE4_37', sk=valk)
    endif
!
!-------------------------------------------------------------------C
!--                                                               --C
!-- RECUPERATION DU NOMBRE DE STRUCTURES DISJOINTES A L'INTERFACE --C
!--      AINSI QUE LES CONNECTIVITES PAR DECOMPOSITION QR         --C
!--                                                               --C
!-------------------------------------------------------------------C
!
    call intdis(coint, nnoint, noddli, '&&MODINT.INTERFACES_SST ', nbsst)
!
    call jeveuo('&&MODINT.INTERFACES_SST ', 'L', lindin)
!
!------------------------------------------------------C
!--                                                  --C
!-- CALCUL DES MODES DU MODELE D'INTERFACE (ARNOLDI) --C
!--                                                  --C
!------------------------------------------------------C
!
!-- ESTIMATION DU NOMBRE DE MODES A CALCULER PAR SOUS STRUCURE
!
    norm=dble(nbmod/nbsst)
    temp=6.d0/nbsst
!
    coeff=3
    if (norm .gt. 7) then
        nbvect=coeff*(int(norm)+2*(int(temp)+1))
    else
        nbvect=coeff*(6+int(norm)+2)
    endif
    nsekry=int(nbvect*nbsst/coeff)
    coeff=int(nbvect/coeff)
    write(ifm,*)'------------------------------------------------',&
     &'------------------------'
    write(ifm,*)' VOUS AVEZ DEMANDE',nbmod,' MODES'
    write(ifm,*)' LA TAILE DU SOUS ESPACE RETENU EST',nsekry
    write(ifm,*)'------------------------------------------------',&
     &'------------------------'
!--
!-- Appel a nmop45 pour le calcul des modes du modele d'interface
!-- 
    bande(1)=0.d0
    bande(2)=1.d0
    nmopt='PLUS_PETITE'
    modes='&&MODEST'
    dummy1='&&DUMY1'
    dummy3='&&DUMMY3'
    call nmop45(imped, ssami, 0, nmopt, nsekry,&
                  2, bande, 'VIBR', dummy1, 0,&
                  modes, '&&DUMMY2', dummy3, 0)
!    
    call wkvect('&&MODINT.SE_KRYLOV', 'V V R', neq*nsekry, lmakry)
    call jeveuo('&&MOIN93.V_IND_LAG', 'L', vi=v_ind_lag)
    call jeveuo('&&MOIN93.DDL_ACTIF_INT', 'L', vi=ddl_actif_int)
!
    do j1 = 1, nsekry
      ! construction du nom du mode "a la main"
      call codent(j1-1, 'D0', k6bid)
      call jeveuo(modes//'.001.'//k6bid//'.VALE', 'L', vr=vale)
      norm=ddot(6*nnoint,vale,1,vale,1)
      norm=sqrt(norm)
      ! normalisation dans L2
      do i1 = 1, nddlin
          m1=ddl_actif_int(i1)
          zr(lmakry+(j1-1)*neq+v_ind_lag(1+(i1-1)*2)-1)=&
              vale(m1)/norm
          zr(lmakry+(j1-1)*neq+v_ind_lag(1+(i1-1)*2+1)-1)=&
              vale(m1)/norm
      end do
    end do
!
    !call detrsd('RESULTAT',modes)
    !call detrsd('RESULTAT','&&DUMMY2')
    call jedetc('G',modes,1)
    call jedetc('G','&&DUMMY2',1)
    
    
    no=max(nsekry,nbvect)
    AS_ALLOCATE(vr=v_f_pro, size=no)
    AS_ALLOCATE(vi=v_ind_f_pro, size=no)
!
!-- RELEVE STATIQUE DU SOUS ESPACE DE KRYLOV SUR LE MODELE COMPLET
!
    call dismoi('SOLVEUR', raide, 'MATR_ASSE', repk=solveu)
    call resoud(raide, '&&MOIN93.MATPRE', solveu, ' ', nsekry,&
                ' ', ' ', ' ', zr(lmakry), [cbid],&
                ' ', .true._1, 0, iret)         
!
!---------------------------------------------C
!--                                         --C
!-- PROJECTION DE LA MASSE ET DE LA RAIDEUR --C
!--                                         --C
!---------------------------------------------C
!
    call wkvect('&&MODINT.M_PROJ_TEMP', 'V V R', neq*nsekry, lmatrm)
    call wkvect('&&MODINT.K_PROJ_TEMP', 'V V R', neq*nsekry, lmatrk)
    call wkvect('&&MODINT.M_PROJ', 'V V R', nsekry**2, lmapro)
    call wkvect('&&MODINT.K_PROJ', 'V V R', nsekry**2, lkpro)
!
!-- MISE A 0 DES DDL DE LAGRANGE
    call dismoi('NOM_NUME_DDL', raide, 'MATR_ASSE', repk=nume)
    call dismoi('PROF_CHNO', nume, 'NUME_DDL', repk=prno)
    call jeveuo(prno//'.DELG', 'L', vi=delg)
!
    do i1 = 1, neq
        if (delg(i1) .lt. 0) then
            do j1 = 1, nsekry
                zr(lmakry+(j1-1)*neq+i1-1)=0.d0
            end do
        endif
    end do
!
    call jeveuo(masse(1:19)//'.&INT', 'L', lmatm)
    call mrmult('ZERO', lmatm, zr(lmakry), zr(lmatrm), nsekry,&
                .true._1)
    call jeveuo(raide(1:19)//'.&INT', 'L', lmatk)
    call mrmult('ZERO', lmatk, zr(lmakry), zr(lmatrk), nsekry,&
                .true._1)
!
    do j1 = 1, nsekry
        zr(lmapro+(j1-1)*nsekry+j1-1)= ddot(neq,zr(lmakry+(j1-1)*neq),&
        1, zr(lmatrm+(j1-1)*neq),1)
        zr(lkpro+(j1-1)*nsekry+j1-1)= ddot(neq,zr(lmakry+(j1-1)*neq),&
        1, zr(lmatrk+(j1-1)*neq),1)
        do i1 = 1, j1-1
            zr(lmapro+(j1-1)*nsekry+i1-1)= ddot(neq,zr(lmakry+(i1-1)*&
            neq),1, zr(lmatrm+(j1-1)*neq),1)
            zr(lmapro+(i1-1)*nsekry+j1-1)= zr(lmapro+(j1-1)*nsekry+i1-&
            1)
!
            zr(lkpro+(j1-1)*nsekry+i1-1)= ddot(neq,zr(lmakry+(i1-1)*&
            neq),1, zr(lmatrk+(j1-1)*neq),1)
            zr(lkpro+(i1-1)*nsekry+j1-1)= zr(lkpro+(j1-1)*nsekry+i1-1)
        end do
    end do
!
!-------------------------------------------------C
!--                                             --C
!-- RESOLUTION DU PB AUX VALEURS PROPRES REDUIT --C
!--                                             --C
!-------------------------------------------------C
!
    AS_ALLOCATE(vr=vect_alphar, size=nsekry)
    AS_ALLOCATE(vr=vect_alphai, size=nsekry)
    AS_ALLOCATE(vr=vect_beta, size=nsekry)
    AS_ALLOCATE(vr=matr_mod_red, size=nsekry**2)
!
    call dggev('N', 'V', nsekry, zr(lkpro), nsekry,&
               zr(lmapro), nsekry, vect_alphar, vect_alphai, vect_beta,&
               matr_mod_red, nsekry, matr_mod_red, nsekry, swork,&
               -1, info)
    lwork=int(swork(1))
    AS_ALLOCATE(vr=matr_work_dggev, size=lwork)
    call dggev('N', 'V', nsekry, zr(lkpro), nsekry,&
               zr(lmapro), nsekry, vect_alphar, vect_alphai, vect_beta,&
               matr_mod_red, nsekry, matr_mod_red, nsekry, matr_work_dggev,&
               lwork, info)
!-- ON REACTIVE LE TEST FPE
    call matfpe(1)
!
!-- CLASSEMENT DES FREQUENCES PROPRES
    temp=1.d+16
    do i1 = 1, nsekry
        if (abs(vect_beta(i1)) .gt. 0) then
            lambda=vect_alphar(i1)/vect_beta(i1)
            v_f_pro(i1)=(sqrt(abs(lambda)))/2/pi
        else
            v_f_pro(i1)=temp
        endif
    end do
!
!
    call jeexin(vefreq,iret)
    if (iret .eq. 0) then
      call wkvect(vefreq, 'V V R', nbmod, lvp)
    else
      ! appel depuis modexp => il faut redimensionner l'objet
      call jedetr(vefreq)
      call wkvect(vefreq, 'V V R', nbmod, lvp)
    endif  
!
    do i1 = 1, nbmod
        temp=1.d+16
        do j1 = 1, nsekry
            if (v_f_pro(j1) .lt. temp) then
                temp=v_f_pro(j1)
                v_ind_f_pro(i1)=j1
            endif
        end do
        v_f_pro(1+v_ind_f_pro(i1)-1)=1.d+16
        lambda=vect_alphar(1+v_ind_f_pro(i1)-1)/ vect_beta(1+v_ind_f_pro(i1)-&
        1)-0*shift
        zr(lvp+i1-1)=(sqrt(abs(lambda)))/2/pi
    end do
!
!-------------------------------------------------------C
!--                                                   --C
!-- RESTITUTION DES MODES D'INTERFACE SUR LE MAILLAGE --C
!--                                                   --C
!-------------------------------------------------------C
!
    call wkvect(matmod, 'V V R', neq*nbmod, lmatmo)
    do j1 = 1, nbmod
        do k1 = 1, nsekry
            temp=matr_mod_red(1+(v_ind_f_pro(j1)-1)*nsekry+k1-1)
            do i1 = 1, neq
                zr(lmatmo+(j1-1)*neq+i1-1)=zr(lmatmo+(j1-1)*neq+i1-1)+&
                temp*zr(lmakry+(k1-1)*neq+i1-1)
            end do
        end do
    end do
!
!---------------------------------------C
!--                                   --C
!-- DESTRUCTION DES OBJETS DE TRAVAIL --C
!--                                   --C
!---------------------------------------C
!
    call jedetr('&&MODINT.M_PROJ_TEMP')
    call jedetr('&&MODINT.K_PROJ_TEMP')
    call jedetr('&&MODINT.M_PROJ')
    call jedetr('&&MODINT.K_PROJ')
    call jedetr('&&MODINT.INTERFACES_SST')
!
    AS_DEALLOCATE(vr=vect_alphar)
    AS_DEALLOCATE(vr=vect_alphai)
    AS_DEALLOCATE(vr=vect_beta)
    AS_DEALLOCATE(vr=matr_mod_red)
    AS_DEALLOCATE(vr=matr_work_dggev)
!
!
    call detrsd('MATR_ASSE', imped)
!
!    call jedetr('&&MODINT.VECT_TEMP')
!    AS_DEALLOCATE(vr=vect_temp_2)
!    call jedetr('&&MODINT.KRYLOV_INT')
    call jedetr('&&MODINT.SE_KRYLOV')
!    AS_DEALLOCATE(vr=hessenberg)
!
!    AS_DEALLOCATE(vr=leftmodes)
!    AS_DEALLOCATE(vr=rightmodes)
!    AS_DEALLOCATE(vr=realpart)
!    AS_DEALLOCATE(vr=imagpart)
    AS_DEALLOCATE(vr=v_f_pro)
    AS_DEALLOCATE(vi=v_ind_f_pro)
!
!-- FIN --C
!
    call jedema()
end subroutine
