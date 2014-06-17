subroutine calcyc(nomres)
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
    implicit none
!
!  BUT:  CALCUL DES MODES CYCLIC OU RECOPIE DE CEUX DEJA EXISTANT
!   DANS UN EVENTUEL CALCUL DE MODES CYCLIQUES PRECEDANT
!
! NOMRES  /I/: NOM UTILISATEUR DU CONCEPT RESULTAT
!
!
#include "jeveux.h"
#include "asterfort/askcyc.h"
#include "asterfort/asmcyc.h"
#include "asterfort/axacti.h"
#include "asterfort/cmphdi.h"
#include "asterfort/cmphii.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/shiftc.h"
#include "asterfort/utmess.h"
#include "asterfort/uttrii.h"
#include "asterfort/wkvect.h"
#include "asterfort/zconju.h"
#include "asterfort/zreord.h"
!
    integer :: vali(3)
!
!
    character(len=6) :: pgc
    character(len=8) :: nomres, typint, basmod
    character(len=14) :: option
    character(len=24) :: repmat, soumat
    character(len=24) :: valk
    complex(kind=8) :: comshi
    logical :: axok
    real(kind=8) :: rlome2(2)
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iad, ibid, icomp, icone, idia, idiam
    integer :: if, imes, ldfre, ldmoc, ldnbd, llitmp
    integer ::  llnum,   lteig, ltkcom, ltlax0
    integer :: ltlax1, ltlbid, ltmcom, ltnbd, ltrv1, ltrv2, lttrge
    integer :: ltzm1, ltzm2, ltzv1, ltzv2, ltzv3, maxdia, nbdax
    integer :: nbdax0, nbdax1, nbddef, nbddg, nbddr, nbdia, nbdia1
    integer :: nbdia2, nblif, nbmcal, nbmobt, nbmos, nbnew, nbsec
    integer :: nbtmp, nmaxit, ntail, ntt, numa
    real(kind=8) :: beta, omeg2, pi, pima, precaj, precse
    integer, pointer :: cycl_nbsc(:) => null()
    character(len=8), pointer :: cycl_type(:) => null()
    character(len=24), pointer :: cycl_refe(:) => null()
!
!-----------------------------------------------------------------------
    data pgc /'CALCYC'/
!-----------------------------------------------------------------------
!
!
    call jemarq()
    pi=4.d0*atan(1.d0)
    llitmp=1
    imes=iunifi('MESSAGE')
!
    soumat='&&OP0080.CYCLIC.SOUS.MAT'
    repmat='&&OP0080.CYCLIC.REPE.MAT'
!
!-----------------RECUPERATION DU TYPE D'INTERFACE----------------------
!
    call jeveuo(nomres//'.CYCL_TYPE', 'L', vk8=cycl_type)
    typint=cycl_type(1)
!
!-----------------RECUPERATION DU NOMBRE DE SECTEURS--------------------
!
    call jeveuo(nomres//'.CYCL_NBSC', 'L', vi=cycl_nbsc)
    nbsec=cycl_nbsc(1)
    maxdia=int((nbsec+1)/2)
!
!----------RECUPERATION DU NOMBRE DE DIAMETRES NODAUX EN COMMANDE-------
!
    call getvis('CALCUL', 'NB_DIAM', iocc=1, nbval=0, nbret=nbdia1)
    nbdia1=-nbdia1
    call getvtx('CALCUL', 'TOUT_DIAM', iocc=1, nbval=0, nbret=nbdia2)
    nbdia2=-nbdia2
!
    if (nbdia2 .gt. 0) then
        nbdia2=int((nbsec+1)/2)+1
    endif
!
!
!  NOMBRE BRUT DE DIAMETRES MODAUX
!
    nbdia=nbdia1+nbdia2
!
!-------------ALLOCATION DU VECTEUR TEMPORAIRE DES DIAMETRES MODAUX-----
!
    call wkvect('&&'//pgc//'.DIAM.TOUT', 'V V I', nbdia, ltnbd)
!
!-------------------RECUPERATION DES DIAMETRES MODAUX-------------------
!
    if (nbdia1 .ne. 0) then
        call getvis('CALCUL', 'NB_DIAM', iocc=1, nbval=nbdia1, vect=zi(ltnbd),&
                    nbret=ibid)
    endif
!
    if (nbdia2 .ne. 0) then
        do 10 i = 1, nbdia2
            zi(ltnbd+nbdia1+i-1)=i-1
10      continue
    endif
!
!
!-----------------TRI DES VALEURS DES DIAMETRES MODAUX------------------
!
    nbnew = nbdia
    if (nbnew .ne. 0) call uttrii(zi(ltnbd), nbnew)
!
    nbdia=nbnew
!
    icomp=0
    do 30 i = 1, nbnew
        idia=zi(ltnbd+i-1)
        if (idia .le. maxdia) then
            icomp=icomp+1
        else
            vali (1) = idia
            call utmess('I', 'ALGORITH14_82', si=vali(1))
        endif
30  end do
!
    if (icomp .lt. nbdia) then
        vali (1) = maxdia
        call utmess('I', 'ALGORITH14_83', si=vali(1))
    endif
!
    nbdia=icomp
    if (nbdia .eq. 0) then
        call utmess('F', 'ALGORITH14_84')
    endif
!
!---------ALLOCATION DU VECTEUR DES NOMBRES DE DIAMETRES MODAUX---------
!
    call wkvect(nomres//'.CYCL_DIAM', 'G V I', nbdia*2, ldnbd)
!
    do 40 i = 1, nbdia
        zi(ldnbd+i-1)=zi(ltnbd+i-1)
40  end do
!
    call jedetr('&&'//pgc//'.DIAM.TOUT')
!
!----------------RECUPERATION DU TYPE DE METHODE------------------------
!
    call getvtx('CALCUL', 'OPTION', iocc=1, scal=option, nbret=ibid)
    call getvis('CALCUL', 'NMAX_ITER', iocc=1, scal=nmaxit, nbret=ibid)
    call getvr8('CALCUL', 'PREC_AJUSTE', iocc=1, scal=precaj, nbret=ibid)
    call getvr8('CALCUL', 'PREC_SEPARE', iocc=1, scal=precse, nbret=ibid)
!
    comshi=dcmplx(0.d0,0.d0)
!
    call getvr8('CALCUL', 'FREQ', iocc=1, nbval=0, nbret=nblif)
    nblif=-nblif
    if (option .eq. 'PLUS_PETITE' .or. option .eq. 'CENTRE') then
        if (nblif .gt. 1) then
            vali (1) = nblif
            valk = option
            call utmess('F', 'ALGORITH14_85', sk=valk, si=vali(1))
        else if (nblif.eq.1) then
            call getvr8('CALCUL', 'FREQ', iocc=1, nbval=nblif, vect=rlome2,&
                        nbret=ibid)
            rlome2(1)=(rlome2(1)*2.d0*pi)**2
            comshi=dcmplx(rlome2(1),0.d0)
        else
            comshi=dcmplx(0.d0,0.d0)
        endif
    else if (option.eq.'BANDE') then
        if (nblif .ne. 2) then
            vali (1) = nblif
            valk = option
            call utmess('F', 'ALGORITH14_85', sk=valk, si=vali(1))
        else
            call getvr8('CALCUL', 'FREQ', iocc=1, nbval=nblif, vect=rlome2,&
                        nbret=ibid)
            rlome2(1)=(rlome2(1)*2.d0*pi)**2
            rlome2(2)=(rlome2(2)*2.d0*pi)**2
        endif
    endif
!
!--------RECUPERATION NOMBRE (PROJECTION) MODES ET DDL LIAISON----------
!              ET NOMBRE DE MODES A CALCULER
!
    call jeveuo(nomres//'.CYCL_DESC', 'L', llnum)
    nbmos=zi(llnum)
    nbddr=zi(llnum+1)
    nbdax=zi(llnum+2)
    nbmcal=zi(llnum+3)
    nbddg=nbmos+nbddr+nbdax
!
!---------RECUPERATION DES DONNEES ASSEMBLAGE PARTIEL DDL AXE-----------
!
    ltlbid = 1
    if (nbdax .gt. 0) then
        call jeveuo(nomres//'.CYCL_NUIN', 'L', llnum)
        numa=zi(llnum+2)
        call jeveuo(nomres//'.CYCL_REFE', 'L', vk24=cycl_refe)
        basmod=cycl_refe(3)(1:8)
!
        call axacti(basmod, numa, 0, [ibid], 0,&
                    nbdax0)
        if (nbdax0 .gt. 0) then
            call wkvect('&&'//pgc//'.LISTE.AXE0', 'V V I', nbdax0, ltlax0)
            call axacti(basmod, numa, 0, zi(ltlax0), nbdax0,&
                        ibid)
        endif
        call axacti(basmod, numa, 1, [ibid], 0,&
                    nbdax1)
        if (nbdax1 .gt. 0) then
            call wkvect('&&'//pgc//'.LISTE.AXE1', 'V V I', nbdax1, ltlax1)
            call axacti(basmod, numa, 1, zi(ltlax1), nbdax1,&
                        ibid)
        endif
        ntt=max(nbmos,nbddr)
        call wkvect('&&'//pgc//'.LISTE.BIDON', 'V V I', ntt, ltlbid)
        do 5 i = 1, ntt
            zi(ltlbid+i-1)=i
 5      continue
    endif
!
!
!--------------------ALLOCATION DES OBJETS RESULTAT---------------------
!
    ntail=nbmcal*nbdia
    call wkvect(nomres//'.CYCL_FREQ', 'G V R', ntail, ldfre)
    ntail=nbdia*nbmcal*nbddg
    call wkvect(nomres//'.CYCL_CMODE', 'G V C', ntail, ldmoc)
!
!--------------ALLOCATION OBJET DE TRAVAIL POUR CALCUL DES MODES--------
!
!
    ntail = nbddg*(nbddg+1)/2
    call wkvect('&&'//pgc//'COMPRAID', 'V V C', ntail, ltkcom)
    call wkvect('&&'//pgc//'COMPMASS', 'V V C', ntail, ltmcom)
    call wkvect('&&'//pgc//'TRAV.GENE', 'V V C', nbddg, lttrge)
!
    call wkvect('&&'//pgc//'.MAT.TRAV1', 'V V C', ntail, ltzm1)
    call wkvect('&&'//pgc//'.MAT.TRAV2', 'V V C', nbddg*nbddg, ltzm2)
    call wkvect('&&'//pgc//'.VEC.TRAV1', 'V V C', nbddg, ltzv1)
    call wkvect('&&'//pgc//'.EIGE', 'V V C', nbmcal, lteig)
!
    if (option .eq. 'BANDE') then
        call wkvect('&&'//pgc//'.VEC.TRAV2', 'V V C', nbddg, ltzv2)
        call wkvect('&&'//pgc//'.VEC.TRAV3', 'V V C', nbddg, ltzv3)
        call wkvect('&&'//pgc//'.VER.TRAV1', 'V V R', nbddg+1, ltrv1)
        call wkvect('&&'//pgc//'.VER.TRAV2', 'V V R', nbddg+1, ltrv2)
    endif
!
!---------------------------IMPRESSIONS DIMENSIONS---------------------
!
    vali (1) = nbmos
    vali (2) = nbddr
    call utmess('I', 'ALGORITH14_87', ni=2, vali=vali)
    if (nbdax .gt. 0) then
        vali (1) = nbdax
        vali (2) = nbdax0
        vali (3) = nbdax1
        call utmess('I', 'ALGORITH14_88', ni=3, vali=vali)
    endif
    vali (1) = nbddg
    call utmess('I', 'ALGORITH14_89', si=vali(1))
!
!
!---------------------------CALCUL DES MODES PROPRES--------------------
!
!  COMPTEUR DES MODES PROPRES COMPLEXES
!
!  ICONE MODES ECRITS DANS NOMRES
!
    icone=0
!
    do 80 i = 1, nbdia
!
        nbmobt=nbmcal
        idiam=zi(ldnbd+i-1)
        beta=(2.d0*pi/nbsec)*idiam
!
!  DETERMINATION DU NOMBRE DE DDL GENERALISES EFFICACE ET INDICATEUR
!    DE PRISE EN COMPTE DES DDL GENERALISES RELATIF A L'AXE  (AXOK)
!  ( SELON PRESENCE DDL AXE ET DIAMETRE MODAUX ET TYPE D'INTERFACE)
!
!
!  DETERMINATION  DES  POINTEUR TEMPORAIRES POUR ASSEMBLAGE
!    DES EVENTUELS DDL AXE: AXOK,LLITMP,NBTMP
!
! CAS CRAIG-BAMPTON
        if (typint .eq. 'CRAIGB   ' .or. typint .eq. 'CB_HARMO') then
            if (nbdax .gt. 0 .and. idiam .eq. 0) then
                nbddef=nbmos+nbddr+nbdax0
                axok=.true.
                llitmp=ltlax0
                nbtmp=nbdax0
            else if (nbdax.gt.0.and.idiam.eq.1) then
                nbddef=nbmos+nbddr+nbdax1
                axok=.true.
                llitmp=ltlax1
                nbtmp=nbdax1
            else
                axok=.false.
                nbddef=nbmos+nbddr
                nbtmp=0
            endif
!
! CAS MAC NEAL OU AUCUN
!
        else
            if (nbdax .gt. 0 .and. idiam .eq. 0) then
                nbddef=nbmos+nbddr+nbdax1
                axok=.true.
                llitmp=ltlax1
                nbtmp=nbdax1
            else if (nbdax.gt.0.and.idiam.eq.1) then
                nbddef=nbmos+nbddr+nbdax0
                axok=.true.
                llitmp=ltlax0
                nbtmp=nbdax0
            else
                axok=.false.
                nbddef=nbmos+nbddr
                nbtmp=0
            endif
        endif
!
!
        call asmcyc(zc(ltmcom), nbddef, soumat, beta, nbmos,&
                    nbddr, nbdax, axok, zi(llitmp), nbtmp,&
                    zi(ltlbid))
!
        call askcyc(zc(ltkcom), nbddef, soumat, beta, nbmos,&
                    nbddr, nbdax, axok, zi(llitmp), nbtmp,&
                    zi(ltlbid))
!
        call shiftc(zc(ltkcom), zc(ltmcom), nbddef, comshi)
!
!
        iad=ldmoc+(nbddg*icone)
!
        if (option .eq. 'PLUS_PETITE' .or. option .eq. 'CENTRE') then
!
            call cmphii(zc(ltkcom), zc(ltmcom), nbddef, nbmobt, nmaxit,&
                        precaj, zc(lteig), zc(iad), nbddg, zc(ltzm1),&
                        zc(ltzm2), zc( ltzv1), imes)
!
        else if (option.eq.'BANDE') then
            call cmphdi(zc(ltkcom), zc(ltmcom), nbddef, nbmobt, nmaxit,&
                        precaj, zc(lteig), zc(iad), nbddg, zc(ltzm1),&
                        zc(ltzm2), zc(ltzv1), zc(ltzv2), zr(ltrv1), zr(ltrv2),&
                        rlome2(1), rlome2( 2), precse)
!
        endif
!
!--------------RECUPERATION DES FREQUENCES PROPRES REELLES--------------
!
        do 110 if = 1, nbmobt
            zc(lteig+if-1)=zc(lteig+if-1)-comshi
            call zconju(zc(lteig+if-1), omeg2, pima)
            if (omeg2 .ge. 0) then
                zr(ldfre+icone+if-1)=(omeg2**0.5d0)/(2.d0*pi)
            else
                zr(ldfre+icone+if-1)=-((-omeg2)**0.5d0)/(2.d0*pi)
            endif
110      continue
!
!
!--------------REORGANISATION DES DDL GENERALISEE-----------------------
!                (DDL AXE ASSEMBLES PARTIELLEMENT)
!
        call zreord(zc(iad), nbddg, nbmobt, nbmos, nbddr,&
                    axok, zi(llitmp), nbtmp, zc(lttrge))
!
!C
        icone=icone+nbmobt
        zi(ldnbd+nbdia+i-1)=nbmobt
!
80  end do
!
!
!   GRAND MENAGE DE PRINTEMPS !!!
!
    if (nbdax .gt. 0) then
        call jedetr('&&'//pgc//'.LISTE.BIDON')
        if (nbdax0 .gt. 0) call jedetr('&&'//pgc//'.LISTE.AXE0')
        if (nbdax1 .gt. 0) call jedetr('&&'//pgc//'.LISTE.AXE1')
    endif
!
    call jedetr('&&'//pgc//'COMPRAID')
    call jedetr('&&'//pgc//'COMPMASS')
    call jedetr('&&'//pgc//'TRAV.GENE')
    call jedetr('&&'//pgc//'.MAT.TRAV1')
    call jedetr('&&'//pgc//'.MAT.TRAV2')
    call jedetr('&&'//pgc//'.VEC.TRAV1')
    call jedetr('&&'//pgc//'.EIGE')
!
    if (option .eq. 'BANDE') then
        call jedetr('&&'//pgc//'.VEC.TRAV2')
        call jedetr('&&'//pgc//'.VEC.TRAV3')
        call jedetr('&&'//pgc//'.VER.TRAV1')
        call jedetr('&&'//pgc//'.VER.TRAV2')
    endif
!
    call jedetr(soumat)
    call jedetr(repmat)
!
    call jedema()
end subroutine
