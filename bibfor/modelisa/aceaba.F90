subroutine aceaba(noma, nomo, lmax, nbarre, nbocc,&
                  mclf, nbtel, ntyele, ivr, ifm,&
                  zjdlm)
!
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
!
implicit none
!
    integer :: lmax, nbarre, nbocc, nbtel, ifm, zjdlm(*)
    integer :: ntyele(*), ivr(*)
    character(len=8) :: noma, nomo
    character(len=*) :: mclf
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/acedat.h"
#include "asterfort/affbar.h"
#include "asterfort/alcart.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/getvem.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jecrec.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nocart.h"
#include "asterfort/utmess.h"
!
! --------------------------------------------------------------------------------------------------
!
!              AFFE_CARA_ELEM : AFFECTATION DES CARACTERISTIQUES POUR L'ELEMENT BARRE
!
! --------------------------------------------------------------------------------------------------
! IN  :
!       NOMA    : Nom du maillage
!       NOMO    : Nom du modèle
!       LMAX    : Nombre max de maille ou groupe de maille
!       NBARRE  : Nombre de barre du modele
!       NBOCC   : Nombre d'occurences du mot clef BARRE
!       MCLF    : Ici c'est 'BARRE'
!       NBTEL   : Nombre total d'élément
!       NTYELE  : Tableau des types d'éléments
!       IVR     : Tableau des indices de vérification
!       IFM     : Unité pour l'écriture des caractéristiques des barres données en entrées
!       JDLM    : Adresse des mailles
! --------------------------------------------------------------------------------------------------
    integer :: i, idw, ier, iisec, ioc, isec, itabl, iarg
    integer :: ivect, ixma, j
    integer :: jdcba, jdcbaf, jdge, jdgef, jdgm
    integer :: jdme, jdvba, jdvbaf
    integer :: k, nbaaff, nbcar, nbcolo, nblign
    integer :: nbmagr, nbmail, nbo, nbval, ncar, ndim, nfcx
    integer :: ng, nm, nnosec, nsec, ntab, ntypse, nummai
    integer :: nutyel, nval
! --------------------------------------------------------------------------------------------------
    character(len=6) :: kioc
    character(len=8) :: k8b, nomu, nommai, fcx
    character(len=16) :: k16b, sec, concep, cmd
    character(len=16) :: vmessk(2)
    character(len=19) :: cartba, cartbf, tabcar
    character(len=24) :: tmpnba, tmpvba, tmpgen, nomsec, typca
    character(len=24) :: tmpnbf, tmpvbf, tmpgef, modmai, mlggma, mlgnma
! --------------------------------------------------------------------------------------------------
    integer, pointer :: tab_para(:) => null()
    integer, pointer :: tbnp(:) => null()
    real(kind=8), pointer :: vale(:) => null()
    character(len=8), pointer :: expbar(:) => null()
    character(len=8), pointer :: carbar(:) => null()
    character(len=8), pointer :: cara(:) => null()
    character(len=8), pointer :: barre2(:) => null()
    character(len=8), pointer :: tabbar(:) => null()
    character(len=16), pointer :: typ_sect(:) => null()
    character(len=24), pointer :: barre(:) => null()
    character(len=24), pointer :: tblp(:) => null()
! --------------------------------------------------------------------------------------------------
    call jemarq()
    call getres(nomu, concep, cmd)
!
    AS_ALLOCATE(vi=tab_para, size=10)
    call acedat('BARRE', 0, tab_para, k16b, k8b, k8b, k8b)
    ntypse = tab_para(2)
    nbo = tab_para(3)
    nbcar = tab_para(4)
    nbval = tab_para(5)
    ndim = tab_para(7) * ntypse
    AS_ALLOCATE(vk16=typ_sect, size=ntypse)
    AS_ALLOCATE(vk8=expbar, size=nbo)
    AS_ALLOCATE(vk8=tabbar, size=nbo)
    AS_ALLOCATE(vk8=carbar, size=ndim)
    call acedat('BARRE', 1, tab_para, typ_sect, expbar, tabbar, carbar)
    AS_ALLOCATE(vk8=cara, size=nbcar)
    AS_ALLOCATE(vr=vale, size=nbval)
!
    modmai = nomo//'.MAILLE'
    mlgnma = noma//'.NOMMAI'
    mlggma = noma//'.GROUPEMA'
    ier = 0
    call jelira(mlgnma, 'NOMMAX', nbmail)
    call jeexin(modmai, ixma)
    if (ixma .ne. 0) call jeveuo(modmai, 'L', jdme)
!
! --------------------------------------------------------------------------------------------------
!   construction des cartes
    tmpgen = nomu//'.BARRE'
    cartba = nomu//'.CARGENBA'
    tmpnba = cartba//'.NCMP'
    tmpvba = cartba//'.VALV'
!
    tmpgef = nomu//'.VENT'
    cartbf = nomu//'.CVENTCXF'
    tmpnbf = cartbf//'.NCMP'
    tmpvbf = cartbf//'.VALV'
!
!   creation d un objet tampon (surdimensionne a NBO*NBARRE)
    call jecrec(tmpgen, 'V V R', 'NO', 'CONTIG', 'CONSTANT', nbarre)
    call jeecra(tmpgen, 'LONMAX', nbo)
    call jecrec(tmpgef, 'V V K8', 'NO', 'CONTIG', 'CONSTANT', nbarre)
    call jeecra(tmpgef, 'LONMAX', 1)
    AS_ALLOCATE(vk24=barre, size=lmax)
    AS_ALLOCATE(vk8=barre2, size=lmax)
!
! --------------------------------------------------------------------------------------------------
    iisec = 0
!   lecture et stockage des donnees  dans l objet tampon
    do ioc = 1, nbocc
        call codent(ioc, 'G', kioc)
        call getvem(noma, 'GROUP_MA', 'BARRE', 'GROUP_MA', ioc,  iarg, lmax, barre, ng)
        call getvem(noma, 'MAILLE', 'BARRE', 'MAILLE', ioc, iarg, lmax, barre2, nm)
        call getvtx('BARRE', 'SECTION', iocc=ioc, scal=sec, nbret=nsec)
        call getvid('BARRE', 'TABLE_CARA', iocc=ioc, scal=tabcar, nbret=ntab)
        if (ntab .eq. 1) then
            call getvtx('BARRE', 'NOM_SEC', iocc=ioc, scal=nomsec, nbret=nnosec)
            ASSERT(nnosec.eq.1)
            call jeveuo(tabcar//'.TBNP', 'L', vi=tbnp)
!            NOMBRE DE CARACTERISTIQUES
            nbcolo = tbnp(1)
!            ON RECHERCHE NOMSEC DANS LA 1ER COLONNE
            call jeveuo(tabcar//'.TBLP', 'L', vk24=tblp)
            typca=tblp(2)
            if (typca(1:2) .ne. 'K8' .and. typca(1:3) .ne. 'K24') then
                call utmess('F', 'MODELISA8_17', sk=tabcar)
            endif
            call jeveuo(tblp(3), 'L', itabl)
            nblign = tbnp(2)
            if (typca .eq. 'K8') then
                do i = 1, nblign
                    if (zk8(itabl-1+i) .eq. nomsec) then
                        iisec=i
                        goto 97
                    endif
                enddo
            else
                do i = 1, nblign
                    if (zk24(itabl-1+i)(1:8) .eq. nomsec) then
                        iisec=i
                        goto 97
                    endif
                enddo
            endif
            vmessk(1)=tabcar(1:16)
            vmessk(2)=nomsec(1:16)
            call utmess('F', 'MODELISA8_18', nk=2, valk=vmessk)
97          continue
!
            do i = 1, nbcolo-1
                if (tblp(1+4*i+1) .ne. 'R') cycle
                if (tblp(1+4*i) .ne. 'A') then
                    cycle
                else
                    cara = tblp(1+4*i)(1:8)
                    call jeveuo(tblp(1+4*i+2), 'L', ivect)
                    vale=zr(ivect-1+iisec)
                    exit
                endif
            enddo
        else
            call getvtx('BARRE', 'CARA', iocc=ioc, nbval=nbcar, vect=cara, nbret=ncar)
            call getvr8('BARRE', 'VALE', iocc=ioc, nbval=nbval, vect=vale, nbret=nval)
            ASSERT(ncar.gt.0)
            if(ncar.ne.nval)then
               call utmess('F','MODELISA10_19',si=ioc)
            endif
        endif
        fcx = '.'
        call getvid('BARRE', 'FCX', iocc=ioc, scal=fcx, nbret=nfcx)
!
        if (sec .eq. typ_sect(1)) isec = 0
        if (sec .eq. typ_sect(1+1)) isec = 1
        if (sec .eq. typ_sect(1+2)) isec = 2
!
!       "GROUP_MA" = TOUTES LES MAILLES POSSIBLES DE LA LISTE DES GROUPES DE MAILLES
        if (ng .gt. 0) then
            do i = 1, ng
                call jeveuo(jexnom(mlggma, barre(i)), 'L', jdgm)
                call jelira(jexnom(mlggma, barre(i)), 'LONUTI', nbmagr)
                groupe: do j = 1, nbmagr
                    nummai = zi(jdgm+j-1)
                    call jenuno(jexnum(mlgnma, nummai), nommai)
                    nutyel = zi(jdme+nummai-1)
                    do k = 1, nbtel
                        if (nutyel .eq. ntyele(k)) then
                            call affbar(tmpgen, tmpgef, fcx, nommai, isec,&
                                        cara, vale, expbar, nbcar, kioc,&
                                        ier)
                            cycle groupe
                        endif
                    enddo
                    vmessk(1) = mclf
                    vmessk(2) = nommai
                    call utmess('F', 'MODELISA_8', nk=2, valk=vmessk)
                enddo groupe
            enddo
        endif
!
!       "MAILLE" = TOUTES LES MAILLES POSSIBLES DE LA LISTE DE MAILLES
        if (nm .gt. 0) then
            maille: do i = 1, nm
                nommai = barre2(i)
                call jenonu(jexnom(mlgnma, nommai), nummai)
                nutyel = zi(jdme+nummai-1)
                do j = 1, nbtel
                    if (nutyel .eq. ntyele(j)) then
                        call affbar(tmpgen, tmpgef, fcx, nommai, isec,&
                                    cara, vale, expbar, nbcar, kioc,&
                                    ier)
                        cycle maille
                    endif
                enddo
                vmessk(1) = mclf
                vmessk(2) = nommai
                call utmess('F', 'MODELISA_8', nk=2, valk=vmessk)
            enddo maille
        endif
!
    enddo
    if (ier .ne. 0) then
        call utmess('F', 'MODELISA_7')
    endif
!
    call jelira(tmpgen, 'NUTIOC', nbaaff)
!
! --------------------------------------------------------------------------------------------------
!   IMPRESSION DES VALEURS AFFECTEES DANS LE TAMPON SI DEMANDE
    if (ivr(3) .eq. 1) then
!       IMPRESSION DES DONNEES GENERALES
        write(ifm,200)
        do i = 1, nbaaff
            call jenuno(jexnum(tmpgen, i), nommai)
            call jeveuo(jexnum(tmpgen, i), 'L', jdge)
            isec = nint(zr(jdge+nbo-1))
            write(ifm,201)nommai,zr(jdge),isec
        enddo
!       IMPRESSION DES DONNEES GEOMETRIQUES
        idw = 0
        do i = 1, nbaaff
            call jenuno(jexnum(tmpgen, i), nommai)
            call jeveuo(jexnum(tmpgen, i), 'L', jdge)
            isec = nint(zr(jdge+nbo-1))
            if (isec .eq. 1) then
                if (idw .eq. 0) then
                    write(ifm,210)
                    idw = 1
                endif
                write(ifm,212)nommai,(zr(jdge+j-1),j=2,5),isec
            else if (isec.eq.2) then
                if (idw .eq. 0) then
                    write(ifm,220)
                    idw = 1
                endif
                write(ifm,222)nommai,(zr(jdge+j-1),j=6,7),isec
            endif
            call jenuno(jexnum(tmpgef, i), nommai)
            call jeveuo(jexnum(tmpgef, i), 'L', jdgef)
            write(ifm,*) 'CX : ', zk8(jdgef)
        enddo
    endif
200 format(/,3x,&
        '<SECTION> VALEURS DE TYPE GENERALE AFFECTEES AUX BARRES'&
        ,//,3x,'MAILLE   A              TSEC')
201 format(3x,a8,1x,1pd12.5,1x,i6)
210 format(/,3x,&
        '<SECTION> VALEURS DE TYPE GEOMETRIQUE AFFECTEES AUX BARRES'&
        ,//,3x,'MAILLE   HY          HZ          EPY         EPZ',&
        '            TSEC')
212 format(3x,a8,1x,4(1pd12.5,1x),i6)
220 format(/,3x,&
        '<SECTION> VALEURS DE TYPE GEOMETRIQUE AFFECTEES AUX BARRES'&
        ,//,3x,'MAILLE   R           EP             TSEC')
222 format(3x,a8,1x,2(1pd12.5,1x),i6)
!
!   ALLOCATION DE LA CARTE
    call alcart('G', cartba, noma, 'CAGNBA')
    call jeveuo(tmpnba, 'E', jdcba)
    call jeveuo(tmpvba, 'E', jdvba)
    call jeveuo(tmpnbf, 'E', jdcbaf)
    call jeveuo(tmpvbf, 'E', jdvbaf)
!
!   AFFECTATIONS DES DONNEES GENERALES
    zk8(jdcba) = tabbar(1)
!   POUR LA CARTE DE VENT ==> FCXP
    zk8(jdcbaf) = 'FCXP'
    do i = 1, nbaaff
        call jenuno(jexnum(tmpgen, i), nommai)
        call jenonu(jexnom(mlgnma, nommai), nummai)
        zjdlm(nummai) = -1
        call jeveuo(jexnum(tmpgen, i), 'L', jdge)
        zr(jdvba) = zr(jdge)
        call jeveuo(jexnum(tmpgef, i), 'L', jdgef)
        zk8(jdvbaf) = zk8(jdgef)
        call nocart(cartba, 3, 1, mode='NOM', nma=1, limano=[nommai])
        call nocart(cartbf, 3, 1, mode='NOM', nma=1, limano=[nommai])
    enddo
!
!   NETTOYAGE
    AS_DEALLOCATE(vk24=barre)
    AS_DEALLOCATE(vk8=barre2)
    AS_DEALLOCATE(vi=tab_para)
    AS_DEALLOCATE(vk16=typ_sect)
    AS_DEALLOCATE(vk8=expbar)
    AS_DEALLOCATE(vk8=tabbar)
    AS_DEALLOCATE(vk8=carbar)
    AS_DEALLOCATE(vk8=cara)
    AS_DEALLOCATE(vr=vale)
    call jedetr(tmpgen)
    call jedetr(tmpgef)
    call jedetr(tmpnba)
    call jedetr(tmpvba)
    call jedetr(tmpnbf)
    call jedetr(tmpvbf)
!
    call jedema()
end subroutine
