subroutine strmag(nugene, typrof)
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
!***********************************************************************
!    P. RICHARD     DATE 02/11/92
!-----------------------------------------------------------------------
!  BUT:      < REFE 127 >
    implicit none
!
!  CREER LE STOCKAGE DU NUME_DDL_GENE
!-----------------------------------------------------------------------
!
! NUGENE   /I/: NOM K14 DU NUME_DDL_GENE
!
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/crsmos.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jevtbl.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/nueq_chck.h"
!
    real(kind=8) :: valr(2)
!
!
!
    character(len=8) :: nomprn, modgen, sst(2)
    character(len=19) :: stomor, prgene
    character(len=14) :: nugene
    character(len=24) :: typrof
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iad1, iad1c, iad1l, iad2l, iadc, iadcou
    integer :: iadl, ibid, ieq, ifimes, ilig, j, jschc, jsmde
    integer :: jsmdi, jsmhc, k, kterm, l, lc, lcolmx
    integer :: lcomoy, lh, ll, lldefl, llorl
    integer :: llors, llprl, llprs,  nbcol, nblig, nbloc
    integer :: nbprno, nbsst, neq, nsstar, ntbloc, nterm
    integer :: ntprno, nuant, nulia, nusst, hcol
    real(kind=8) :: rtbloc
    integer, pointer :: nueq(:) => null()
    integer, pointer :: nequ(:) => null()
    character(len=24), pointer :: refn(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    ifimes=iunifi('MESSAGE')
    stomor=nugene//'.SMOS'
    prgene=nugene//'.NUME'
    call jeveuo(prgene//'.NEQU', 'L', vi=nequ)
    neq=nequ(1)
!
!
    if (typrof .eq. 'PLEIN' .or. typrof .eq. 'DIAG') then
        call crsmos(stomor, typrof, neq)
        goto 999
    endif
!
!
!     -- CAS TYPROF=LIGN_CIEL :
!     --------------------------
    rtbloc=jevtbl('TAILLE_BLOC')
    ntbloc=int(1024*rtbloc)
!
!----------------RECUPERATION DU MODELE GENERALISE----------------------
!          ET NOMBRE DE SOUS-STRUCTURE
    call jeveuo(prgene//'.REFN', 'L', vk24=refn)
    modgen=refn(1)(1:8)
    call jelira(modgen//'      .MODG.SSNO', 'NOMMAX', nbsst)
!
!
!---------------DETERMINATION DU PROFIL(LIGNE DE CIEL)------------------
    call wkvect('&&STRMAG.SCHC', 'V V I', neq, jschc)
!
    call nueq_chck(prgene)
    call jeveuo(prgene//'.NUEQ', 'L', vi=nueq)
    call jelira(prgene//'.PRNO', 'NMAXOC', nbprno)
    nterm = 0
    if (typrof .eq. 'LIGN_CIEL') then
!
!  BOUCLE SUR LIGRELS DU PRNO
        do i = 1, nbprno
            call jelira(jexnum(prgene//'.PRNO', i), 'LONMAX', ntprno)
            ntprno=ntprno/2
            call jenuno(jexnum(prgene//'.LILI', i), nomprn)
!
!   CAS DU PRNO &SOUSSTR (MATRICES PROJETEES)
            if (nomprn .eq. '&SOUSSTR') then
                call jeveuo(jexnum(prgene//'.PRNO', i), 'L', llprs)
                do j = 1, ntprno
                    iad1=zi(llprs+(j-1)*2)
                    nblig=zi(llprs+(j-1)*2+1)
!
!   BOUCLE SUR LES COLONNES DE LA MATRICE PROJETEE
                    do k = 1, nblig
                        iadcou=nueq(1+iad1-1+k-1)
                        lh=jschc+iadcou-1
                        zi(lh)=max(zi(lh),k)
                    end do
                end do
!
!
!    CAS  DU PRNO LAGRANGE D'INTERFACE
            else
                call jeveuo(jexnum(prgene//'.ORIG', i), 'L', llorl)
                call jeveuo(jexnum(prgene//'.PRNO', i), 'L', llprl)
                call jenonu(jexnom(prgene//'.LILI', '&SOUSSTR'), ibid)
                call jeveuo(jexnum(prgene//'.ORIG', ibid), 'L', llors)
                call jenonu(jexnom(prgene//'.LILI', '&SOUSSTR'), ibid)
                call jeveuo(jexnum(prgene//'.PRNO', ibid), 'L', llprs)
!
                do j = 1, ntprno
                    nulia=zi(llorl+j-1)
                    call jeveuo(jexnum(modgen//'      .MODG.LIDF', nulia), 'L', lldefl)
! RECUPERATION DES 2 SOU-STRUCTURES ASSOCIEES
                    sst(1)=zk8(lldefl)
                    sst(2)=zk8(lldefl+2)
                    iad1l=zi(llprl+(j-1)*2)
                    nblig=zi(llprl+(j-1)*2+1)
                    do k = 1, 2
                        call jenonu(jexnom(modgen//'      .MODG.SSNO', sst(k)), nusst)
!  RECUPERATION NUMERO TARDIF
                        do  l = 1, nbsst
                            if (zi(llors+l-1) .eq. nusst) nsstar=l
                        end do
                        iad1c=zi(llprs+(nsstar-1)*2)
                        nbcol=zi(llprs+(nsstar-1)*2+1)
                        do ll = 1, nblig
                            iadl=nueq(1+(iad1l-1)+(ll-1))
                            do lc = 1, nbcol
                                iadc=nueq(1+(iad1c-1)+(lc-1))
                                lh=jschc+max(iadc,iadl)-1
                                zi(lh)=max(zi(lh),abs(iadc-iadl)+1)
                            end do
                        end do
                    end do
!
! TRAITEMENT DES MATRICES LAGRANGE-LAGRANGE
!
! RECUPERATION DU NUMERO NOEUD TARDIF ANTAGONISTE
                    do l = 1, ntprno
                        if (zi(llorl+l-1) .eq. nulia .and. l .ne. j) nuant=l
                    end do
                    iad2l=zi(llprl+(nuant-1)*2)
                    do ll = 1, nblig
                        iadl=nueq(1+(iad1l-1)+(ll-1))
                        iadc=nueq(1+(iad2l-1)+(ll-1))
! TERME CROISE LAGRANGE LAGRANGE
                        lh=jschc+max(iadc,iadl)-1
                        zi(lh)=max(zi(lh),abs(iadc-iadl)+1)
! TERME DIAGONAL LAGRANGE LAGRANGE
                        lh=jschc+iadl-1
                        zi(lh)=max(zi(lh),1)
                    end do
                end do
            endif
        end do
    else if (typrof.eq.'PLEIN') then
        do i = 1, neq
            zi(jschc+i-1)=i
        end do
    endif
!
!---------------DETERMINATION DE LA TAILLE MAX D'UNE COLONNE------------
    lcomoy=0
    lcolmx=0
    do i = 1, neq
        lcolmx=max(lcolmx,zi(jschc+i-1))
        lcomoy=lcomoy+zi(jschc+i-1)
    end do
!
    lcomoy=lcomoy/neq
!
    if (lcolmx .gt. ntbloc) then
        ntbloc=lcolmx
        valr (1) = rtbloc
        valr (2) = lcolmx/1.d+3
        call utmess('I', 'ALGORITH14_66', nr=2, valr=valr)
    endif
!
    write(ifimes,*)'+++ HAUTEUR MAXIMUM D''UNE COLONNE: ',lcolmx
    write(ifimes,*)'+++ HAUTEUR MOYENNE D''UNE COLONNE: ',lcomoy
!
!----------------DETERMINATION DU NOMBRE DE TERMES-----------------------
!----------------STOCKAGE OBJETS .SMDI .SMHC      -----------------------
!
    call wkvect(stomor//'.SMDI', 'G V I', neq, jsmdi)
    nbloc=1
    nterm=0
    do ieq = 1, neq
        hcol = zi(jschc+ieq-1)
        nterm=nterm+hcol
        zi(jsmdi-1+ieq)=nterm    
    end do
! 
    write(ifimes,*)'+++ NOMBRE DE BLOCS DU STOCKAGE: ',nbloc
    write(ifimes,*)'+++ NOMBRE DE TERMES DU STOCKAGE: ',nterm
!   
    call wkvect(stomor//'.SMHC', 'G V S', nterm, jsmhc)
    kterm=0
    do ieq = 1, neq
        hcol = zi(jschc+ieq-1)
        ASSERT(hcol.le.ieq)
        do ilig=ieq-hcol+1,ieq
           kterm=kterm+1
           zi4(jsmhc-1+kterm)=ilig
        end do 
    end do
!
!     -- .SMDE
!
    call wkvect(stomor//'.SMDE', 'G V I', 6, jsmde)
    zi(jsmde-1+1)=neq
    zi(jsmde-1+2)=nterm
    zi(jsmde-1+3)=1
!
999 continue
    call jedema()
end subroutine
