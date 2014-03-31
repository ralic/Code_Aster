subroutine regene(nomres, resgen, profno)
    implicit none
#include "jeveux.h"
#include "asterc/gettco.h"
#include "asterfort/copmod.h"
#include "asterfort/dcapno.h"
#include "asterfort/dismoi.h"
#include "asterfort/genugl.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mdgepc.h"
#include "asterfort/mdgeph.h"
#include "asterfort/refdaj.h"
#include "asterfort/refdcp.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/rsorac.h"
#include "asterfort/titre.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcrea.h"
#include "asterfort/vtcreb.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: nomres, resgen
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!  BUT : < RESTITUTION GENERALISEE >
!
!  RESTITUER EN BASE PHYSIQUE LES RESULTATS "MODE_GENE"
!  SANS SOUS-STRUCTURATION
!  LE CONCEPT RESULTAT EST UN RESULTAT COMPOSE "MODE_MECA"
!               OU "MODE_MECA_C"
!-----------------------------------------------------------------------
!
! NOMRES /I/ : NOM K8 DU CONCEPT MODE MECA RESULTAT
! RESGEN /I/ : NOM K8 DU MODE_GENE AMONT
!
!
!
!
!
    integer :: i, j, iadref, iarefe, ibid, idbase, ier, iord, iret, iret1, itresu
    integer :: jbid, ldnew, llchol, llinsk, llnueq, nbmod, nbnot, neq
    integer :: nno, iadpar(13), iadpas(13), nbmo2, llref2, llref3, llref4, llref5, tmod(1)
    real(kind=8) :: rbid
    complex(kind=8) :: cbid
    character(len=1) :: typsca
    character(len=8) :: basmod, respro, kbid, k8b, modmec, mailsk, modgen
    character(len=14) :: numddl
    character(len=16) :: depl, nompar(13), typrep
    character(len=19) :: chamno, kint, chamne, raid, numgen, profno
    character(len=24) :: chamol, indirf, crefe(2), numedd, basmo2
    character(len=24) :: valk, matric(3)
    logical :: zcmplx
!
!-----------------------------------------------------------------------
    data depl   /'DEPL            '/
    data nompar /'FREQ','RIGI_GENE','MASS_GENE','OMEGA2',&
     &           'AMOR_REDUIT',&
     &           'FACT_PARTICI_DX','FACT_PARTICI_DY','FACT_PARTICI_DZ',&
     &           'MASS_EFFE_DX','MASS_EFFE_DY','MASS_EFFE_DZ',&
     &           'NUME_MODE',&
     &           'TYPE_MODE'/
!-----------------------------------------------------------------------
    call jemarq()
!
!-----RECUPERATION NOMBRE DE MODES PROPRES CALCULES---------------------
!
    zcmplx = .false.
!
    call dcapno(resgen, depl, 1, chamol)
    call jelira(chamol, 'TYPE', cval=typsca)
    if (typsca .eq. 'C') zcmplx = .true.
!
    call rsorac(resgen, 'LONUTI', 0, rbid, kbid,&
                cbid, rbid, kbid, tmod, 1,&
                ibid)
    nbmod=tmod(1)
!
! --- ON RESTITUE SUR TOUS LES MODES OU SUR QUELQUES MODES:
!
    call getvis(' ', 'NUME_ORDRE', nbval=0, nbret=nno)
    if (nno .ne. 0) then
        nbmod = -nno
        call wkvect('&&REGENE.NUME', 'V V I', nbmod, jbid)
        call getvis(' ', 'NUME_ORDRE', nbval=nbmod, vect=zi(jbid), nbret=nno)
    else
        call wkvect('&&REGENE.NUME', 'V V I', nbmod, jbid)
        do i = 1, nbmod
            zi(jbid+i-1) = i
        end do
    endif
!
! --- ALLOCATION STRUCTURE DE DONNEES RESULTAT
!
    if (zcmplx) then
        call rscrsd('G', nomres, 'MODE_MECA_C', nbmod)
    else
        call rscrsd('G', nomres, 'MODE_MECA', nbmod)
    endif
!
! --- RECUPERATION DE LA BASE MODALE
!
    call jeveuo(jexnum(resgen//'           .TACH', 1), 'L', iarefe)
    kint = zk24(iarefe)(1:19)
    call jeveuo(kint//'.VALE', 'L', itresu)
    call jeveuo(kint//'.REFE', 'L', iadref)
    basmod = zk24(iadref)(1:8)
!
    basmo2 = basmod
    call gettco(basmo2, typrep)
!
    if (typrep(1:9) .eq. 'MODE_GENE') then
        call getvid(' ', 'SQUELETTE', scal=mailsk, nbret=ibid)
        indirf = '&&REGEGL'//'.INDIR.SST'
!
! ------ VERIF SQUELETTE
!
        call jeexin(mailsk//'.INV.SKELETON', iret)
        if (iret .eq. 0) then
            valk = mailsk
            call utmess('F', 'ALGORITH14_27', sk=valk)
        endif
        call jeveuo(mailsk//'.INV.SKELETON', 'L', llinsk)
!
! ------ RECUPERATION DU MODELE GENERALISE
!
        call dismoi('REF_RIGI_PREM', resgen, 'RESU_DYNA', repk=raid)
!
        call jeveuo(raid//'.REFA', 'L', llref2)
        numgen(1:14)=zk24(llref2+1)
        numgen(15:19)='.NUME'
        call jelibe(raid//'.REFA')
!
        call jeveuo(numgen//'.REFN', 'L', llref3)
        respro=zk24(llref3)
        call jelibe(numgen//'.REFN')
!
        call dismoi('REF_RIGI_PREM', respro, 'RESU_DYNA', repk=raid)
!
        call jeveuo(raid//'.REFA', 'L', llref4)
        numgen(1:14)=zk24(llref4+1)
        numgen(15:19)='.NUME'
        call jelibe(raid//'.REFA')
!
        call jeveuo(numgen//'.REFN', 'L', llref5)
        modgen=zk24(llref5)
        call jelibe(numgen//'.REFN')
!
! ------ CREATION DU PROF-CHAMNO
!
        call genugl(profno, indirf, modgen, mailsk)
        call jelira(profno//'.NUEQ', 'LONMAX', neq)
!
! ------ RECUPERATION DU NOMBRE DE NOEUDS
!
        call dismoi('NB_NO_MAILLA', mailsk, 'MAILLAGE', repi=nbnot)
!
! ------ RECUPERATION DE LA BASE MODALE
!
        crefe(1)=mailsk
        crefe(2)=profno
!
!C
!CC ---- RESTITUTION PROPREMENT DITE
!C
!
        call jeveuo(numgen//'.NUEQ', 'L', llnueq)
        call getvid(' ', 'MODE_MECA', scal=modmec, nbret=ibid)
        if (ibid .ne. 0) basmod=modmec
        call rsorac(basmod, 'LONUTI', 0, rbid, kbid,&
                    cbid, rbid, kbid, tmod, 1,&
                    ibid)
        nbmo2=tmod(1)
        call wkvect('&&REGENE.BASEMODE', 'V V R', nbmo2*neq, idbase)
        call copmod(basmod,numer=profno(1:14),bmodr=zr(idbase), nequa=neq)
!
! ------ BOUCLE SUR LES MODES A RESTITUER
!
        do i = 1, nbmod
            iord = zi(jbid+i-1)
!
! --------- REQUETE NOM ET ADRESSE CHAMNO GENERALISE
!
            call dcapno(resgen, depl, iord, chamol)
            call jeveuo(chamol, 'L', llchol)
!
! --------- REQUETE NOM ET ADRESSE NOUVEAU CHAMNO
!
            call rsexch(' ', nomres, depl, i, chamne,&
                        ier)
            if (zcmplx) then
                call vtcrea(chamne, crefe, 'G', 'C', neq)
            else
                call vtcrea(chamne, crefe, 'G', 'R', neq)
            endif
            call jeveuo(chamne//'.VALE', 'E', ldnew)
!
            call rsadpa(resgen, 'L', 13, nompar, iord,&
                        0, tjv=iadpar, styp=kbid)
!
            if (zcmplx) then
                call mdgepc(neq, nbmo2, zr(idbase), zc(llchol), zc( ldnew))
            else
                call mdgeph(neq, nbmo2, zr(idbase), zr(llchol), zr( ldnew))
            endif
!
            call rsnoch(nomres, depl, i)
            call rsadpa(nomres, 'E', 13, nompar, i,&
                        0, tjv=iadpas, styp=kbid)
            do j=1,11
               zr(iadpas(j)) = zr(iadpar(j))
            enddo
            zi(iadpas(12)) = zi(iadpar(12))
            zk16(iadpas(13)) = 'MODE_DYN'
!
            call jelibe(chamol)
        end do
!-----------------------------------------------------------------------
    else
!-----------------------------------------------------------------------
!
!
        call rsorac(basmod, 'LONUTI', 0, rbid, kbid,&
                    cbid, rbid, kbid, tmod, 1,&
                    ibid)
        nbmo2=tmod(1)
!
        call dismoi('NUME_DDL', basmod, 'RESU_DYNA', repk=numedd)
        call getvid(' ', 'NUME_DDL', scal=k8b, nbret=iret1)
        if (iret1 .ne. 0) then
            call getvid(' ', 'NUME_DDL', scal=numedd, nbret=ibid)
            numedd = numedd(1:14)//'.NUME'
        endif
        numddl = numedd(1:14)
        call dismoi('NB_EQUA', numddl, 'NUME_DDL', repi=neq)
        call wkvect('&&REGENE.BASEMODE', 'V V R', nbmo2*neq, idbase)
        call copmod(basmod, numer=numddl, bmodr=zr(idbase), nequa=neq)
!
!CC ---- RESTITUTION PROPREMENT DITE
!C
!
        do i = 1, nbmod
            iord = zi(jbid+i-1)
!
! --------- REQUETE NOM ET ADRESSE CHAMOL GENERALISE
!
            call dcapno(resgen, depl, iord, chamol)
            call jeveuo(chamol, 'L', llchol)
!
! --------- REQUETE NOM ET ADRESSE NOUVEAU CHAMNO
!
            call rsexch(' ', nomres, depl, i, chamno,&
                        ier)
            if (zcmplx) then
                call vtcreb(chamno, numedd, 'G', 'C', neq)
            else
                call vtcreb(chamno, numedd, 'G', 'R', neq)
            endif
            call jeveuo(chamno//'.VALE', 'E', ldnew)
!
            call rsadpa(resgen, 'L', 13, nompar, iord,&
                        0, tjv=iadpar, styp=kbid)
!
            if (zcmplx) then
                call mdgepc(neq, nbmo2, zr(idbase), zc(llchol), zc( ldnew))
            else
                call mdgeph(neq, nbmo2, zr(idbase), zr(llchol), zr( ldnew))
            endif
!
            call rsnoch(nomres, depl, i)
!
            call rsadpa(nomres, 'E', 13, nompar, i,&
                        0, tjv=iadpas, styp=kbid)
            do j=1,11
               zr(iadpas(j)) = zr(iadpar(j))
            enddo
            zi(iadpas(12)) = zi(iadpar(12))
            zk16(iadpas(13)) = 'MODE_DYN'
!
            call jelibe(chamol)
        end do
!
        if (iret1 .ne. 0) then
            matric(1) = ' '
            matric(2) = ' '
            matric(3) = ' '
            call refdaj('F', nomres, nbmod, numedd, 'DYNAMIQUE',&
                        matric, ier)
        else
            call refdcp(basmod, nomres)
        endif
    endif
!
! --- MENAGE
    call jedetr('&&REGENE.NUME')
    call jedetr('&&REGENE.BASEMODE')
    call jedetr('&&REGEGL'//'.INDIR.SST')
!
    call titre()
    call jedema()
end subroutine
