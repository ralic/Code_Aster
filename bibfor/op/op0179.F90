subroutine op0179()
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!
!     OPERATEUR LIRE_FORC_MISS
!
!-----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/irmifr.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ulisop.h"
#include "asterfort/ulopen.h"
#include "asterfort/wkvect.h"
    integer :: ibid, n1, n2, n4, nbmode
    real(kind=8) :: partr, parti, coef, dpi
    character(len=8) :: nomres, basemo, numgen
    character(len=16) :: typres, nomcom, typbas, k16nom, tissf
    character(len=19) :: resu
    character(len=19) :: nomnum, nomsto
    character(len=24) :: tabrig, tabri2
    character(len=72) :: texte
    character(len=255) :: fichi
    character(len=2) :: nomcmp
    character(len=4) :: nomcha
    real(kind=8) :: a(3), a2(3)
    logical(kind=1) :: lissf
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, i1, iadesc, iarefe, iavale, ic, icf
    integer :: ifmis, ifreq, ii, j, jri2, jrig
    integer :: nbmodd, nbmods, nbmodt, nc, nf, nfr, nfreq
    integer :: nsau0, nsaut
    real(kind=8) :: freq
    integer, pointer :: scde(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    call infmaj()
!
    call getres(nomres, typres, nomcom)
!
! --- RECUPERATION DES ARGUMENTS DE LA COMMANDE
!
    call getvis(' ', 'UNITE_RESU_FORC', scal=ifmis, nbret=n1)
    call getvtx(' ', 'NOM_CMP', scal=nomcmp, nbret=nc)
    if (nc .eq. 0) then
        call getvis(' ', 'NUME_CHAR', scal=ic, nbret=n1)
    endif
    call getvtx(' ', 'NOM_CHAM', scal=nomcha, nbret=ibid)
    call getvr8(' ', 'FREQ_EXTR', scal=freq, nbret=nfr)
    lissf = .false.
    call getvtx(' ', 'ISSF', scal=tissf, nbret=n2)
    if (tissf(1:3) .eq. 'OUI') lissf = .true.
    call getvtx(' ', 'NOM_RESU_FORC', scal=fichi, nbret=nf)
    if (nf .eq. 0) then
        k16nom = ' '
        if (ulisop ( ifmis, k16nom ) .eq. 0) then
            call ulopen(ifmis, ' ', ' ', 'NEW', 'O')
        endif
    else
        call ulopen(ifmis, fichi, ' ', 'NEW', 'O')
    endif
    call irmifr(ifmis, freq, ifreq, nfreq, icf)
    write(6,*) 'FREQ NFREQ= ',freq,nfreq,' IFREQ= ',ifreq,' IF= ',icf
    call getvid(' ', 'BASE', scal=basemo, nbret=n4)
    call getvid(' ', 'NUME_DDL_GENE', scal=numgen, nbret=n2)
!
    call gettco(basemo, typbas)
!
    nomnum = numgen//'      .NUME'
    nomsto = numgen//'      .SLCS'
    dpi = 8.d0*atan2(1.d0,1.d0)
!
!==================================================
!
!
! RECUPERATION DU NOMBRE DE MODES REDUIT,
! NB_VECT DONNE PAR NUME_DDL_GENE
!      NBMODE   = ZI(JSCDE-1+1)
!
    call dismoi('NB_MODES_DYN', basemo, 'RESULTAT', repi=nbmodd)
    call dismoi('NB_MODES_STA', basemo, 'RESULTAT', repi=nbmods)
!
    if (lissf) then
        nbmode = nbmodd + nbmods
    else
        nbmode = nbmods
    endif
    tabrig = '&&OP0179.RIGM'
    tabri2 = '&&OP0179.RIG2'
    call wkvect(tabrig, 'V V R', 2*nbmode, jrig)
    call wkvect(tabri2, 'V V R', 2*nbmode, jri2)
    rewind ifmis
    read(ifmis,'(A72)') texte
    if (texte(1:4) .eq. 'XXXX') goto 4
    nsau0 = 0
    if (nc .ne. 0) then
        if (nomcmp .eq. 'DY') nsau0 = (nfreq+1)*nbmode
        if (nomcmp .eq. 'DZ') nsau0 = 2*(nfreq+1)*nbmode
    else
        nsau0 = (ic-1)*(nfreq+1)*nbmode
    endif
    do i1 = 1, nbmode
        nsaut = nfreq
        if (icf .ge. 1) nsaut = nfreq-1
        if (i1 .eq. 1) nsaut = ifreq + nsau0
        do i = 1, nsaut
            read(ifmis,'(A72)') texte
        end do
        read(ifmis,*) (a(j),j=1,3)
        if (nomcha .eq. 'VITE') then
            coef = -1.d0/(dpi*a(1))
            zr(jrig+2*i1-2) = a(3)*coef
            zr(jrig+2*i1-1) = a(2)*coef
        else
            coef = 1.d0
            if (nomcha .eq. 'ACCE') coef = -1.d0/(dpi*a(1))**2
            zr(jrig+2*i1-2) = a(2)*coef
            zr(jrig+2*i1-1) = -a(3)*coef
        endif
        if (icf .eq. 1) then
            read(ifmis,*) (a2(j),j=1,3)
            if (nomcha .eq. 'VITE') then
                coef = -1.d0/(dpi*freq)
                zr(jrig+2*i1-2)=coef* (a(3)+(freq-a(1))/(a2(1)-a(1))*(&
                a2(3)-a(3)))
                zr(jrig+2*i1-1)=coef* (a(2)+(freq-a(1))/(a2(1)-a(1))*(&
                a2(2)-a(2)))
            else
                coef = 1.d0
                if (nomcha .eq. 'ACCE') coef = -1.d0/(dpi*freq)**2
                zr(jrig+2*i1-2)=coef* (a(2)+(freq-a(1))/(a2(1)-a(1))*(&
                a2(2)-a(2)))
                zr(jrig+2*i1-1)=-coef* (a(3)+(freq-a(1))/(a2(1)-a(1))*&
                (a2(3)-a(3)))
            endif
        endif
        if (icf .eq. 2) then
            read(ifmis,*) (a2(j),j=1,3)
            if (nomcha .eq. 'VITE') then
                coef = -1.d0/(dpi*a2(1))
                zr(jri2+2*i1-2) = a2(3)*coef
                zr(jri2+2*i1-1) = a2(2)*coef
            else
                coef = 1.d0
                if (nomcha .eq. 'ACCE') coef = -1.d0/(dpi*a2(1))**2
                zr(jri2+2*i1-2) = a2(2)*coef
                zr(jri2+2*i1-1) = -a2(3)*coef
            endif
            zr(jrig+2*i1-2) = zr(jrig+2*i1-2) + (freq-a(1))/(a2(1)-a( 1)) * (zr(jri2+2*i1-2)-zr(j&
                              &rig+2*i1-2))
            zr(jrig+2*i1-1) = zr(jrig+2*i1-1) + (freq-a(1))/(a2(1)-a( 1)) * (zr(jri2+2*i1-1)-zr(j&
                              &rig+2*i1-1))
        endif
    end do
  4 continue
!
! ----- RECUPERATION DU NOMBRE D'EQUATIONS DU SYSTEME PHYSIQUE
!
!
    call jeveuo(nomsto//'.SCDE', 'L', vi=scde)
!
    resu = ' '
    resu(1:8) = nomres
!
! --- CREATION DE L OBJET VECT_GENE RESULTAT
!
    nbmodt = nbmodd + nbmods
    call wkvect(resu//'.VALE', 'G V C', nbmodt, iavale)
    call wkvect(resu//'.REFE', 'G V K24', 2, iarefe)
    call wkvect(resu//'.DESC', 'G V I', 3, iadesc)
    call jeecra(resu//'.DESC', 'DOCU', cval='VGEN')
!
! --- REMPLISSAGE DU .REFE ET .VALE
!
    zk24(iarefe) = basemo
    zk24(iarefe+1) = nomnum
    zi(iadesc) = 1
    zi(iadesc+1) = nbmodt
!   ON TESTE LA HAUTEUR MAXIMALE DES COLONNES DE LA MATRICE
!   SI CETTE HAUTEUR VAUT 1, ON SUPPOSE QUE LE STOCKAGE EST DIAGONAL
    if (scde(4) .eq. 1) then
        zi(iadesc+2) = 1
    else
        zi(iadesc+2) = 2
    endif
!
!
    do i = 1, nbmodt
!
        ii = i - nbmodd
        if (lissf .and. i .le. nbmodd) ii = i+nbmods
!
! --------- BOUCLE SUR LES INDICES VALIDES DE LA COLONNE I
!
        if (.not.lissf .and. i .le. nbmodd) then
            zc(iavale+i-1) = dcmplx(0.d0,0.d0)
        else
!
! ----------- STOCKAGE DANS LE .VALE A LA BONNE PLACE
!
            partr = zr(jrig+2*ii-2)
            parti = zr(jrig+2*ii-1)
            zc(iavale+i-1) = dcmplx(partr,parti)
!
        endif
    end do
!
    call jedetr(tabrig)
!
    call jedema()
end subroutine
