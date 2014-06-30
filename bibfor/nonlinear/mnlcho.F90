subroutine mnlcho(reprise, imat, numedd, xcdl, nd,&
                  nchoc, h, hf, parcho, adime,&
                  ninc, tabchoc, lcine)
    implicit none
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
!-----------------------------------------------------------------------
!
!       MODE NON LINEAIRE - STOCKAGE PARAMETRES CHOCS
!       -         -         ---
! ----------------------------------------------------------------------
! IN   REPRISE: L    : .TRUE. SI REPRISE DU CALCUL
! IN   IMAT   : I(2) : DESCRIPTEUR DES MATRICES :
!                       - IMAT(1) => MATRICE DE RAIDEUR
!                       - IMAT(2) => MATRICE DE MASSE
! IN   NUMEDD : K14  : NUME_DDL DES MATRICES DE MASSE ET RAIDEUR
! IN   XCDL   : K14  : INDICE DES CONDITIONS AUX LIMITES
! IN   ND     : I    : NOMBRE DE DEGRES DE LIBERTE
! IN   NCHOC  : I    : NOMBRE DE CONTACTEURS
! IN   H      : I    : NOMBRE D'HARMONIQUES de X
! IN   HF     : I    : NOMBRE D'HARMONIQUES POUR F
! OUT  PARCHO : K14  : SD PARAMETRE DES CONTACTEURS
! OUT  ADIME  : K14  : SD PARAMETRE ADIMENSIONNEMENT
! OUT  NINC   : I    : NOMBRE D INCONNUES DU SYSTEME
! IN   TABCHOC: K8   : SI REPRISE=.TRUE. ALORS TABCHOC CONTIENT LES PARAMETRES DES CONTACTEURS
! IN   LCINE  : L    : .TRUE. SI AFFE_CHAR_CINE
! ----------------------------------------------------------------------
!
!
#include "jeveux.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/mrmult.h"
#include "asterfort/mtcmbl.h"
#include "asterfort/mtdefs.h"
#include "asterfort/posddl.h"
#include "asterfort/preres.h"
#include "asterfort/resoud.h"
#include "asterfort/tbexve.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "blas/ddot.h"
#include "blas/dscal.h"
! ----------------------------------------------------------------------
! --- DECLARATION DES ARGUMENTS DE LA ROUTINE
! ----------------------------------------------------------------------
    logical(kind=1) :: reprise, lcine
    integer :: imat(2), nd, nchoc, h, hf, ninc
    character(len=14) :: numedd, xcdl, parcho, adime
    character(len=8) :: tabchoc
!
! ----------------------------------------------------------------------
! --- DECLARATION DES VARIABLES LOCALES
! ----------------------------------------------------------------------
    integer ::       iind, k, ier, pddl, ind, ldgn
    integer :: j, nunoe, iadim, pdlmax, neq, iei, ier2, i,  iorig
    integer ::    icmp, icmp1, icmp2, iorigx, iorigy, iorigz
    character(len=19) :: matk, matm, nomcmp1, nomcmp2, origx, origy, origz
    character(len=8) :: tchoc, kvide, typval, mailla
    character(len=8) :: noeud(2)
    character(len=8) :: cmp(6)
    character(len=24) :: magrno, manono, grno
    real(kind=8) :: orig(3)
    complex(kind=8) :: cbid
    real(kind=8), pointer :: ei2(:) => null()
    real(kind=8), pointer :: reg(:) => null()
    character(len=8), pointer :: noeu(:) => null()
    real(kind=8), pointer :: raid(:) => null()
    real(kind=8), pointer :: jeu(:) => null()
    real(kind=8), pointer :: jeumax(:) => null()
    character(len=8), pointer :: type(:) => null()
    integer, pointer :: neqs(:) => null()
    integer, pointer :: indmax(:) => null()
    integer, pointer :: nddl(:) => null()
    integer, pointer :: ncmp(:) => null()
    cbid = dcmplx(0.d0, 0.d0)
!
    call jemarq()
!
! ----------------------------------------------------------------------
! --- INITIALISATION DU NOMBRE TOTAL D'INCONNUE DU SYSTEME
! ----------------------------------------------------------------------
    ninc=nd*(2*h+1)
    if (reprise) then
        nomcmp1 = '&&NOM_CMP_1'
        nomcmp2 = '&&NOM_CMP_2'
        origx = '&&ORIG_X'
        origy = '&&ORIG_Y'
        origz = '&&ORIG_Z'
        call tbexve(tabchoc, 'TYPE_CHOC', parcho//'.TYPE', 'V', nchoc,&
                    typval)
        call tbexve(tabchoc, 'NOEUD_CHOC', parcho//'.NOEU', 'V', nchoc,&
                    typval)
        call tbexve(tabchoc, 'NOM_CMP_1', nomcmp1, 'V', nchoc,&
                    typval)
        call tbexve(tabchoc, 'NOM_CMP_2', nomcmp2, 'V', nchoc,&
                    typval)
        call tbexve(tabchoc, 'RIGI_NOR', parcho//'.RAID', 'V', nchoc,&
                    typval)
        call tbexve(tabchoc, 'PARA_REGUL', parcho//'.REG', 'V', nchoc,&
                    typval)
        call tbexve(tabchoc, 'JEU', parcho//'.JEU', 'V', nchoc,&
                    typval)
        call tbexve(tabchoc, 'ORIG_OBST_X', origx, 'V', nchoc,&
                    typval)
        call tbexve(tabchoc, 'ORIG_OBST_Y', origy, 'V', nchoc,&
                    typval)
        call tbexve(tabchoc, 'ORIG_OBST_Z', origz, 'V', nchoc,&
                    typval)
        call jeveuo(nomcmp1, 'L', icmp1)
        call jeveuo(nomcmp2, 'L', icmp2)
        call jeveuo(origx, 'L', iorigx)
        call jeveuo(origy, 'L', iorigy)
        call jeveuo(origz, 'L', iorigz)
    endif
!
! ----------------------------------------------------------------------
! --- RECUPERATION DES CHAMPS DE PARCHO A REMPLIR
! ----------------------------------------------------------------------
    call jeveuo(parcho//'.JEUMAX', 'E', vr=jeumax)
    call jeveuo(parcho//'.INDMAX', 'E', vi=indmax)
    call jeveuo(parcho//'.NDDL', 'E', vi=nddl)
    call jeveuo(parcho//'.NEQS', 'E', vi=neqs)
    call jeveuo(parcho//'.NCMP', 'E', vi=ncmp)
    call jeveuo(parcho//'.CMP', 'E', icmp)
    call jeveuo(parcho//'.ORIG', 'E', iorig)
    call jeveuo(parcho//'.TYPE', 'E', vk8=type)
    call jeveuo(parcho//'.NOEU', 'E', vk8=noeu)
    call jeveuo(parcho//'.RAID', 'E', vr=raid)
    call jeveuo(parcho//'.REG', 'E', vr=reg)
    call jeveuo(parcho//'.JEU', 'E', vr=jeu)
! ----------------------------------------------------------------------
! --- RECUPERATION : NOM DE LA MATRICE DE RAIDEUR ET DU VECTEUR D'INDICE
! ----------------------------------------------------------------------
    matk=zk24(zi(imat(1)+1))(1:19)
    matm=zk24(zi(imat(2)+1))(1:19)
    call jeveuo(xcdl, 'L', iind)
! ----------------------------------------------------------------------
! --- BOUCLE SUR LE NOMBRE DE NOEUD DE CHOC
! ----------------------------------------------------------------------
    do k = 1, nchoc
! --- RECUPERATION DU TYPE DE CHOC DU NOEUD
        if (reprise) then
            tchoc = type(k)
        else
            call getvtx('CHOC', 'OBSTACLE', iocc=k, scal=tchoc)
        endif
        type(k)=tchoc
! --- RECUPERATION DU NBRE D EQ SUPP ET DE COMPOSANTE POUR CHAQUE NOEUD
        if (tchoc(1:7) .eq. 'BI_PLAN') then
            ncmp(k)=1
            neqs(k)=2
        else if (tchoc(1:4).eq.'PLAN') then
            ncmp(k)=1
            neqs(k)=1
        else if (tchoc(1:6).eq.'CERCLE') then
            ncmp(k)=2
            neqs(k)=4
            orig=0.d0
            if (reprise) then
                orig(1) = zr(iorigx-1+k)
                orig(2) = zr(iorigy-1+k)
                orig(3) = zr(iorigz-1+k)
            else
                call getvr8('CHOC', 'ORIG_OBST', iocc=k, nbval=3, vect=orig)
            endif
        endif
! --- ACTUALISATION DU NOMBRE TOTAL D'INCONNUE DU SYSTEME
        ninc=ninc+neqs(k)*(2*hf+1)
!
! --- RECUPERATION DU NOM DU NOEUD
        if (reprise) then
            noeud(1) = noeu(k)
        else
            call getvtx('CHOC', 'GROUP_NO', iocc=k, scal=grno, nbret=ier)
            if (ier .eq. 1) then
                call dismoi('NOM_MAILLA', matm, 'MATR_ASSE', repk=mailla)
                magrno = mailla//'.GROUPENO'
                manono = mailla//'.NOMNOE'
                call jelira(jexnom(magrno, grno), 'LONUTI', ier, kvide)
                call jeveuo(jexnom(magrno, grno), 'L', ldgn)
                call jenuno(jexnum(manono, zi(ldgn)), noeud(1))
            else
                call getvtx('CHOC', 'NOEUD', iocc=k, scal=noeud(1))
            endif
        endif
        noeu(k)=noeud(1)
! --- RECUPERATION DU NOM DE LA COMPOSANTE DU NOEUD
        if (reprise) then
            if (tchoc(1:6) .eq. 'CERCLE') then
                ier = 2
                cmp(1) = zk8(icmp1-1+k)
                cmp(2) = zk8(icmp2-1+k)
            else
                ier = 1
                cmp(1) = zk8(icmp1-1+k)
            endif
        else
            call getvtx('CHOC', 'NOM_CMP', iocc=k, nbval=ncmp(k), vect=cmp,&
                        nbret=ier)
        endif
        do i = 1, ier
            zk8(icmp-1+2*(k-1)+i) = cmp(i)
        end do
!
        do i = 1, ncmp(k)
            if (tchoc(1:7) .eq. 'CERCLE') then
                if (cmp(i) .eq. 'DX') then
                    zr(iorig-1+3*(k-1)+i)=orig(1)
                else if (cmp(i).eq.'DY') then
                    zr(iorig-1+3*(k-1)+i)=orig(2)
                else if (cmp(i).eq.'DZ') then
                    zr(iorig-1+3*(k-1)+i)=orig(3)
                endif
            endif
! --- RECUPERATION DU NUMERO D'EQUATION DU NOEUD A LA BONNE COMPOSANTE
            call posddl('NUME_DDL', numedd, noeud(1), cmp(i), nunoe,&
                        pddl)
! --- RECUPERATION DE LA POSITION DU NOEUD DANS LA MATRICE AVEC DDLS ACTIFS
            ind=0
            do j = 1, pddl
                if (zi(iind-1+j) .eq. 0) then
                    ind=ind+1
                endif
            end do
            nddl(6*(k-1)+i)=ind
        end do
        if (.not. reprise) then
! --- JE RECUPERE LA VALEUR DU JEU ENTRE LE NOEUD ET LA BUTEE
            call getvr8('CHOC', 'JEU', iocc=k, scal=jeu(k))
! --- JE RECUPERE LA VALEUR DE LA RAIDEUR DE BUTEE
            call getvr8('CHOC', 'RIGI_NOR', iocc=k, scal=raid(k))
! --- JE RECUPERE LA VALEUR DU PARAMETRE DE REGULARISATION DE BUTEE
            call getvr8('CHOC', 'PARA_REGUL', iocc=k, scal=reg(k))
        endif
! --- JE RECUPERE LES VALEURS MAXIMALES POUR L'ADIMENSIONNEMENT
        if (jeu(k) .gt. jeumax(1)) then
            jeumax(1)=jeu(k)
            indmax(1)=nddl(k)
            pdlmax=pddl
        endif
    end do
! ----------------------------------------------------------------------
! --- RECUPERATION DES CHAMPS DE ADIME (POUR L'ADIMENSIONNEMENT)
! ----------------------------------------------------------------------
    call jeveuo(adime, 'E', iadim)
    neq = zi(imat(1)+2)
    call wkvect('&&MNLCHO.EI', 'V V R', neq, iei)
! --- ON CREE DEUX MATRICES DE SUBSTITUTION POUR LA FACTORISATION
!        call mtdefs('&&mnlcho.adime.matk', matk, 'V', ' ')
!        call mtcmbl(1, 'R', 1.d0, matk, '&&mnlcho.adime.matk',&
!                ' ', ' ', 'ELIM1')
!        call mtdefs('&&mnlcho.adime.matm', matm, 'V', ' ')
!        call mtcmbl(1, 'R', 1.d0, matm, '&&mnlcho.adime.matm',&
!                ' ', ' ', 'ELIM1')
! --- ON FACTORISE LES DEUX MATRICES DE SUBSTITUTION
!        call preres(' ', 'V', ier, '', '&&mnlcho.adime.matm',&
!                ier2, 0)
!        zr(iei-1+pdlmax)=1.d0
!
! ------------------------------------------------------------------
! --- ON RECUPERE KUi (POUR ADIMENSIONNE LA MATRICE DE RAIDEUR)
    zr(iei-1+pdlmax)=1.d0
    call preres(' ', 'V', ier, '', matk,&
                ier2, 0)
    call resoud(matk, ' ', ' ', ' ', 1,&
                ' ', ' ', 'V', zr(iei), [cbid],&
                ' ', .false._1, 0, ier)
    zr(iadim-1+1)=1.d0/zr(iei-1+pdlmax)
! --- ON RECUPERE MUi (POUR ADIMENSIONNE LA MATRICE DE MASSE)
    if (lcine) then
        call preres(' ', 'V', ier, '', matm,&
                    ier2, 0)
        call dscal(neq, 0.d0, zr(iei), 1)
        zr(iei-1+pdlmax)=1.d0
        call resoud(matm, ' ', ' ', ' ', 1,&
                    ' ', ' ', 'V', zr(iei), [cbid],&
                    ' ', .false._1, 0, ier)
        zr(iadim-1+2)=1.d0/zr(iei-1+pdlmax)
    else
        do k = 1, neq
            zr(iei-1+k)=1.d0
        end do
        AS_ALLOCATE(vr=ei2, size=neq)
!        call mrmult('ZERO', imat(1), zr(iei), ei2, 1, .true._1)
!        zr(iadim-1+1)=ddot(neq,zr(iei),1,ei2,1)
        call dscal(neq, 0.d0, ei2, 1)
        call mrmult('ZERO', imat(2), zr(iei), ei2, 1,&
                    .true._1)
        zr(iadim-1+2)=ddot(neq,zr(iei),1,ei2,1)
    endif
! --- ON RECUPERE OMEGA (POUR ADIMENSIONNE LE TEMPS)
    zr(iadim-1+3)=sqrt(zr(iadim-1+1)/zr(iadim-1+2))
! ----------------------------------------------------------------------
! --- ACTUALISATION DU NOMBRE TOTAL D'INCONNUE DU SYSTEME
! ----------------------------------------------------------------------
    ninc=ninc+4
!
!    call jedetr('&&mnlcho.adime.matk')
!    call jedetr('&&mnlcho.adime.matm')
!    call detrsd('MATR_ASSE','&&mnlcho.adime.matk')
!    call detrsd('MATR_ASSE','&&mnlcho.adime.matm')
    call jedetr('&&MNLCHO.EI')
    AS_DEALLOCATE(vr=ei2)
!
    call jedema()
!
end subroutine
