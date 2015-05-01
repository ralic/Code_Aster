subroutine matimp(matz, ific, typimz)
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
! person_in_charge: jacques.pellet at edf.fr
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
    character(len=*) :: matz, typimz
    integer :: ific
! ---------------------------------------------------------------------
! BUT: IMPRIMER UNE MATRICE SUR UN LISTING
! ---------------------------------------------------------------------
!     ARGUMENTS:
! MATZ   IN/JXIN  K19 : MATR_ASSE A IMPRIMER
! IFIC   IN       I   : NUMERO DE L'UNITE LOGIQUE D'IMPRESSION
! TYPIMP IN       K8  : FORMAT DE L'IMPRESSION ' ', 'ASTER' OU 'MATLAB'
!                       SI TYPIMP=' ', TYPIMP='ASTER'
! ---------------------------------------------------------------------
!
!
!     ------------------------------------------------------------------
    integer :: iligl, jcoll, kterm, n, nz, nsmdi, jsmhc, nsmhc
    integer :: jdelg, n1, nvale, jvale, nlong, jval2, nuno, nucmp, k, jcmp
    integer :: iligg, jcolg, jnlogl, coltmp
    character(len=8) :: nomgd, nocmp, noma, nono, typimp
    character(len=14) :: nonu
    character(len=1) :: ktyp
    character(len=19) :: mat19
    aster_logical :: ltypr, lsym, lmd
    real(kind=8) :: dble, dimag
    integer, pointer :: deeq(:) => null()
    integer, pointer :: smdi(:) => null()
    character(len=24), pointer :: refa(:) => null()
    character(len=24), pointer :: refn(:) => null()
!
!     ------------------------------------------------------------------
    call jemarq()
!
!
    mat19 = matz
    typimp=typimz
!
    call jeveuo(mat19//'.REFA', 'L', vk24=refa)
    noma=refa(1)
    nonu=refa(2)
!
    lmd=.false.
    if (refa(11) .eq. 'MATR_DISTR') lmd=.true.
!
    call jeveuo(nonu//'.SMOS.SMDI', 'L', vi=smdi)
    call jelira(nonu//'.SMOS.SMDI', 'LONMAX', nsmdi)
    call jeveuo(nonu//'.SMOS.SMHC', 'L', jsmhc)
    call jelira(nonu//'.SMOS.SMHC', 'LONMAX', nsmhc)
    if (lmd) then
        call jeveuo(nonu//'.NUML.DELG', 'L', jdelg)
        call jelira(nonu//'.NUML.DELG', 'LONMAX', n1)
        call jeveuo(nonu//'.NUML.NULG', 'L', jnlogl)
    else
        call jeveuo(nonu//'.NUME.DELG', 'L', jdelg)
        call jelira(nonu//'.NUME.DELG', 'LONMAX', n1)
        jnlogl=0
    endif
    ASSERT(n1.eq.nsmdi)
!     --- CALCUL DE N
    n=nsmdi
!     --- CALCUL DE NZ
    nz=smdi(n)
!
    ASSERT(nz.le.nsmhc)
    call jelira(mat19//'.VALM', 'NMAXOC', nvale)
    if (nvale .eq. 1) then
        lsym=.true.
    else if (nvale.eq.2) then
        lsym=.false.
    else
        ASSERT(.false.)
    endif
!
    call jeveuo(jexnum(mat19//'.VALM', 1), 'L', jvale)
    call jelira(jexnum(mat19//'.VALM', 1), 'LONMAX', nlong)
    ASSERT(nlong.eq.nz)
    if (.not.lsym) then
        call jeveuo(jexnum(mat19//'.VALM', 2), 'L', jval2)
        call jelira(jexnum(mat19//'.VALM', 2), 'LONMAX', nlong)
        ASSERT(nlong.eq.nz)
    endif
!
    call jelira(jexnum(mat19//'.VALM', 1), 'TYPE', cval=ktyp)
    ltypr=(ktyp.eq.'R')
!
!     --- ENTETES
    write(ific,*) ' '
    write(ific,*) '% --------------------------------------------'//&
     &                '----------------------------------------------'
!     --- ENTETE FORMAT ASTER
    if ((typimp.eq.' ') .or. (typimp.eq.'ASTER')) then
        write(ific,*) 'DIMENSION DE LA MATRICE :',n
        write(ific,*) 'NOMBRE DE TERMES NON NULS (MATRICE SYMETRIQUE) :'&
     &                ,nz
        write(ific,*) 'MATRICE A COEEFICIENTS REELS :',ltypr
        write(ific,*) 'MATRICE SYMETRIQUE :',lsym
        write(ific,*) 'MATRICE DISTRIBUEE :',lmd
        write(ific,*) ' '
!     --- ENTETE FORMAT MATLAB
    else if (typimp.eq.'MATLAB') then
        write(ific,*) '% IMPRESSION DE LA MATRICE '//mat19//' AU FORMAT'&
     &                //' MATLAB.'
        if (lmd) then
            write(ific,*) '% 0- FUSIONNER LES FICHIERS PROVENANT DES'//&
     &                  ' DIFFERENTS PROCESSEURS (MATR_DISTRIBUEE).'
        endif
        write(ific,*) '% 1- COPIER DANS UN FICHIER mat.dat.'
        write(ific,*) '% 2- CHARGER DANS MATLAB OU OCTAVE PAR :'
        write(ific,*) '% 2-1 >> load -ascii mat.dat;'
        write(ific,*) '% 2-2 >> A=spconvert(mat);'
        write(ific,*) ' '
    else
        ASSERT(.false.)
    endif
!
!
!     ------------------------------------------------
!     IMPRESSION DES TERMES DE LA MATRICE
!     ------------------------------------------------
    if ((typimp.eq.' ') .or. (typimp.eq.'ASTER')) write(ific, 1003) 'ILIGL', 'JCOLL', 'VALEUR'
    jcoll=1
    do 1 kterm = 1, nz
!
!       --- PARTIE TRIANGULAIRE SUPERIEURE
        if (smdi(jcoll) .lt. kterm) jcoll=jcoll+1
        iligl=zi4(jsmhc-1+kterm)
        if (lmd) then
            iligg=zi(jnlogl+iligl-1)
            jcolg=zi(jnlogl+jcoll-1)
        else
            iligg=iligl
            jcolg=jcoll
        endif
        if ((.not.lsym) .and. (iligg.ge.jcolg)) then
            coltmp=jcolg
            jcolg=iligg
            iligg=coltmp
        endif
        if (ltypr) then
            write(ific,1001) iligg,jcolg,zr(jvale-1+kterm)
        else
            write(ific,1002) iligg,jcolg,dble(zc(jvale-1+kterm)),&
            dimag(zc(jvale-1+kterm))
        endif
!
!        --- PARTIE TRIANGULAIRE INFERIEURE
        if ((.not.lsym) .and. (iligg.ne.jcolg)) then
            if (ltypr) then
                write(ific,1001) jcolg,iligg,zr(jval2-1+kterm)
            else
                write(ific,1002) jcolg,iligg,dble(zc(jval2-1+kterm)),&
                dimag(zc(jval2-1+kterm))
            endif
        endif
!
!       --- SI 'MATLAB' ET SYMETRIQUE , PSEUDO PARTIE INFERIEURE
        if (lsym .and. (typimp.eq.'MATLAB') .and. (iligg.ne.jcolg)) then
            if (ltypr) then
                write(ific,1001) jcolg,iligg,zr(jvale-1+kterm)
            else
                write(ific,1002) jcolg,iligg,dble(zc(jvale-1+kterm)),&
                dimag(zc(jvale-1+kterm))
            endif
        endif
!
  1 end do
!
!
!
!
!     -- IMPRESSION DES CARACTERISTIQUES DES EQUATIONS :
!     --------------------------------------------------
!
    if ((typimp.eq.' ') .or. (typimp.eq.'ASTER')) then
        write(ific,*) ' '
        write(ific,*) 'DESCRIPTION DES EQUATIONS :'
        write(ific,*) ' '
        write(ific,*) '   NUM_EQUA NOEUD    CMP'
        call jeveuo(nonu//'.NUME.DEEQ', 'L', vi=deeq)
        call jeveuo(nonu//'.NUME.REFN', 'L', vk24=refn)
        call jelira(nonu//'.NUME.DEEQ', 'LONMAX', n1)
        nomgd=refn(2)
        call jeveuo(jexnom('&CATA.GD.NOMCMP', nomgd), 'L', jcmp)
        ASSERT(n1.eq.2*n)
        do 2 k = 1, n
            nuno=deeq(2*(k-1)+1)
            nucmp=deeq(2*(k-1)+2)
            if (nuno .gt. 0 .and. nucmp .gt. 0) then
                call jenuno(jexnum(noma//'.NOMNOE', nuno), nono)
                nocmp=zk8(jcmp-1+nucmp)
                write(ific,1004) k,nono,nocmp
            else if (nucmp.lt.0) then
                ASSERT(nuno.gt.0)
                call jenuno(jexnum(noma//'.NOMNOE', nuno), nono)
                nocmp=zk8(jcmp-1-nucmp)
                if (zi(jdelg-1+k) .eq. -1) then
                    write(ific,1005) k,nono,nocmp,' LAGR1 BLOCAGE'
                else
                    ASSERT(zi(jdelg-1+k).eq.-2)
                    write(ific,1005) k,nono,nocmp,' LAGR2 BLOCAGE'
                endif
            else
                ASSERT(nuno.eq.0 .and. nucmp.eq.0)
                nono=' '
                nocmp=' '
                if (zi(jdelg-1+k) .eq. -1) then
                    write(ific,1005) k,nono,nocmp,' LAGR1 RELATION LINEAIRE'
                else
                    ASSERT(zi(jdelg-1+k).eq.-2)
                    write(ific,1005) k,nono,nocmp,' LAGR2 RELATION LINEAIRE'
                endif
            endif
  2     continue
    endif
!
!     --- FIN IMPRESSION
    write(ific,*) '% --------------------------------------------'//&
     &              '----------------------------------------------'
!
!
!
    1001 format(2i12,1(1x,1pe23.15))
    1002 format(2i12,2(1x,1pe23.15,1pe23.15))
    1003 format(3a12,1x,1a14)
    1004 format(i12,2(1x,a8))
    1005 format(i12,2(1x,a8),1x,a)
!
    call jedema()
end subroutine
