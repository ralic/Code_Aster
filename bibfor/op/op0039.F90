subroutine op0039()
    implicit   none
! ----------------------------------------------------------------------
! person_in_charge: nicolas.sellenet at edf.fr
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
!     BUT:
!       IMPRIMER DES RESULTATS ET DES MAILLAGE
!       PROCEDURE IMPR_RESU
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getvid.h"
#include "asterc/getvis.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/infmaj.h"
#include "asterfort/irmail.h"
#include "asterfort/irmfac.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mdexma.h"
#include "asterfort/rdtmai.h"
#include "asterfort/rdtres.h"
#include "asterfort/rscrmo.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/ulexis.h"
#include "asterfort/ulisog.h"
#include "asterfort/ulopen.h"
#include "asterfort/w039ca.h"
#include "asterfort/wkvect.h"
    integer :: nocc, iocc, ioc2, nbrest, ifc, ifi, versio, infmai, nive, ier
    integer :: numemo, nbmodl, nmail, nresu, ncham, ibid, nres, n11, iret, ndim
    integer :: jlast, jmodl, iarg, nmo, nn, nmod, nforma, ngibi, ifimed, codret
!
    real(kind=8) :: versi2, eps
!
    character(len=1) :: k1occ, saux01
    character(len=8) :: modele, noma, noma2, form, nomare, nomsq
    character(len=8) :: k8b, resu, nomab, resure(9), resur(9), saux08
    character(len=16) :: fich, formr
    character(len=24) :: nomjv, valk(6), corrn, corrm
    character(len=200) :: nofimd
    character(len=255) :: kfic
!
    logical :: lresu, lcasts, lmod, existm
    logical :: lmail, lrest, lgmsh
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
!
!     --- RECUPERATION DU NOMBRE DE MISES EN FACTEUR DU MOT-CLE RESU ---
    call getfac('RESU', nocc)
!
    do 100 iocc = 1, nocc
        call getvtx('RESU', 'NOEUD_CMP', iocc, iarg, 0,&
                    k8b, nmo)
        if (nmo .ne. 0) then
            nn = nmo / 2
            if (2*nn .ne. nmo) then
                call u2mess('F', 'PREPOST3_65')
            endif
        endif
100  end do
!
!     -----------------
!     --- LE MODELE ---
!     -----------------
    lmod = .false.
    modele = ' '
    call getvid(' ', 'MODELE', 1, iarg, 1,&
                modele, nmod)
    if (nmod .ne. 0) lmod= .true.
!
!     ----------------------------------------------------------------
!     TRAITEMENT DU MOT CLE "RESTREINT"
!     ----------------------------------------------------------------
!     -- REMARQUE : LE MOT CLE RESTREINT CREE DES SD_RESULTAT
!        TEMPORAIRES + 1 MAILLAGE TEMPORAIRE ET CE SONT CES
!        SD QUI DOIVENT ETRE IMPRIMEES.
!        C'EST POURQUOI, DANS LE RESTE DE L'OPERATEUR (QUI IMPRIME),
!        ON DOIT REMPLACER :
!          - GETVID/RESULTAT PAR RESURE(IOCC)
!          - GETVID/MAILLAGE PAR NOMARE
    lrest=.false.
    call getfac('RESTREINT', nbrest)
    ASSERT(nbrest.eq.0.or.nbrest.eq.1)
    nomare=' '
    if (nbrest .eq. 1) then
        lrest=.true.
!       -- SI RESTREINT, IL FAUT VERIFIER QUE RESU/RESULTAT EST
!          TOUJOURS FOURNI :
        ASSERT(nocc.le.9)
        do 74,iocc=1,nocc
        call getvid('RESU', 'RESULTAT', iocc, iarg, 1,&
                    resur(iocc), nresu)
        if (nresu .eq. 0) call u2mess('F', 'CALCULEL4_5')
74      continue
!
!       -- ON VERIFIE QUE TOUS LES RESUR ONT LE MEME MAILLAGE
        call dismoi('F', 'NOM_MAILLA', resur(1), 'RESULTAT', ibid,&
                    noma, ier)
        do 76,iocc=2,nocc
        call dismoi('F', 'NOM_MAILLA', resur(iocc), 'RESULTAT', ibid,&
                    noma2, ier)
        ASSERT(noma2.ne.' ')
        if (noma2 .ne. noma) then
            valk(1)=resur(1)
            valk(2)=resur(iocc)
            valk(3)=noma
            valk(4)=noma2
            call u2mesk('F', 'CALCULEL4_2', 4, valk)
        endif
76      continue
!
!       -- ON VA FABRIQUER :
!          NOMARE : 1 SD_MAILLAGE "RESTREINT"
!          RESURE : NOCC SD_RESULTAT "RESTREINT"
        nomare='&&OP0039'
        corrn='&&OP0039.CORRN'
        corrm='&&OP0039.CORRM'
        call rdtmai(noma, nomare, 'V', corrn, corrm,&
                    'V', 0, 0)
!
        do 77, iocc=1,nocc
        call codent(iocc, 'D', k1occ)
        resure(iocc)='&RESUR'//k1occ//'_'
        call rdtres(resur(iocc), resure(iocc), noma, nomare, corrn,&
                    corrm, iocc)
77      continue
    endif
!
!     ---------------------------------------------
!     --- FORMAT, FICHIER ET UNITE D'IMPRESSION ---
!     ---------------------------------------------
!
!     --- FORMAT ---
    call getvtx(' ', 'FORMAT', 1, iarg, 1,&
                form, nforma)
    if (lrest .and. form .ne. 'MED') call u2mess('F', 'CALCULEL4_3')
!
!     --- VERIFICATION DE LA COHERENCE ENTRE LE MAILLAGE ---
!     --- PORTANT LE RESULTAT ET LE MAILLAGE DONNE PAR   ---
!     --- L'UTILISATEUR DANS IMPR_RESU(FORMAT='IDEAS')   ---
!
    if (form(1:5) .eq. 'IDEAS') then
        call getvid('RESU', 'RESULTAT', 1, iarg, 1,&
                    resu, nres)
        call getvid('RESU', 'MAILLAGE', 1, iarg, 1,&
                    noma, nmail)
        if (nres*nmail .gt. 0) then
            call dismoi('F', 'NOM_MAILLA', resu, 'RESULTAT', ibid,&
                        nomsq, ier)
            if (nomsq .ne. noma) then
                valk(1)=noma
                valk(2)=nomsq
                valk(3)=resu
                call u2mesk('A', 'PREPOST3_74', 3, valk)
            endif
        endif
    endif
!
!
!     --- VERSION D'ECRITURE  ----
    nive = 0
    versio = 0
    lcasts = .false.
    if (form .eq. 'CASTEM') then
        lcasts = .true.
        call getvis(' ', 'NIVE_GIBI', 1, iarg, 1,&
                    nive, ngibi)
    else if (form(1:5) .eq. 'IDEAS') then
        versio = 5
        call getvis(' ', 'VERSION', 1, iarg, 1,&
                    versio, ngibi)
    else if (form(1:4) .eq. 'GMSH') then
        versio = 1
        versi2 = 1.0d0
        eps = 1.0d-6
        call getvr8(' ', 'VERSION', 1, iarg, 1,&
                    versi2, ngibi)
        if (versi2 .gt. 1.0d0-eps .and. versi2 .lt. 1.0d0+eps) then
            versio = 1
        else if (versi2.gt.1.2d0-eps.and.versi2.lt.1.2d0+eps) then
            versio = 2
        endif
    endif
!
!     --- FICHIER ---
    ifi = 0
    fich = 'F_'//form
    call getvis(' ', 'UNITE', 1, iarg, 1,&
                ifi, n11)
    ifc = ifi
    if (.not. ulexis( ifi )) then
        call ulopen(ifi, ' ', fich, 'NEW', 'O')
    endif
!
!     -- VERIFICATION POUR IMPR_RESU RESTREINT
    if (lrest .and. form .eq. 'MED') then
        call ulisog(ifi, kfic, saux01)
        if (kfic(1:1) .eq. ' ') then
            call codent(ifi, 'G', saux08)
            nofimd = 'fort.'//saux08
        else
            nofimd = kfic(1:200)
        endif
        ifimed = 0
        call mdexma(nofimd, ifimed, nomare, 0, existm,&
                    ndim, codret)
        if (existm) call u2mess('F', 'MED2_8')
    endif
!
!     -- FORMAT CASTEM : IMPRESSION DU MAILLAGE :
!     -------------------------------------------
    formr=' '
    if (form .eq. 'CASTEM') then
        numemo = 0
        nomjv = '&&OP0039.NOM_MODELE'
        infmai = 0
        do 200 iocc = 1, nocc
            if (numemo .eq. 0) then
                if (lmod) then
                    nbmodl = 1
                    call wkvect(nomjv, 'V V K24', 10, jmodl)
                    call jeecra(nomjv, 'LONUTI', nbmodl)
                    call jeveuo(nomjv, 'E', jmodl)
                    zk24(jmodl) = modele//'.MODELE'
                endif
                do 202 ioc2 = 1, nocc
                    call getvid('RESU', 'RESULTAT', ioc2, iarg, 1,&
                                resu, nresu)
                    if (nresu .ne. 0) call rscrmo(ioc2, resu, nomjv)
202              continue
                numemo = numemo + 1
            endif
!
!           ---  IMPRESSION DU MAILLAGE -----
            call getvid('RESU', 'MAILLAGE', iocc, iarg, 1,&
                        noma, nmail)
            if (nmail .ne. 0) then
                if (lmod) then
                    call dismoi('C', 'NOM_MAILLA', modele, 'MODELE', ibid,&
                                nomab, iret)
                    if (noma .ne. nomab) call u2mess('F', 'PREPOST3_66')
                endif
                call irmail(form, ifi, versio, noma, lmod,&
                            modele, nive, infmai, formr)
                numemo = numemo + 1
            endif
200      continue
!
        if (numemo .le. 1) call u2mess('F', 'PREPOST3_67')
!
        call jeexin('&&OP0039.LAST', iret)
        if (iret .eq. 0) call wkvect('&&OP0039.LAST', 'V V I', 8, jlast)
    endif
!
!     -- VERIFICATIONS POUR GMSH :
    if (form(1:4) .eq. 'GMSH') then
        lmail=.false.
        lresu=.false.
        do 220 iocc = 1, nocc
            call getvid('RESU', 'MAILLAGE', iocc, iarg, 1,&
                        noma, nmail)
            call getvid('RESU', 'RESULTAT', iocc, iarg, 1,&
                        resu, nresu)
            call getvid('RESU', 'CHAM_GD', iocc, iarg, 1,&
                        resu, ncham)
            if (nresu .ne. 0 .or. ncham .ne. 0) then
                lresu=.true.
                goto 220
            endif
            if (nmail .ne. 0) lmail=.true.
220      continue
        if (lmail .and. lresu) then
            call u2mess('A', 'PREPOST3_68')
            goto 9999
        endif
    endif
    lgmsh = .false.
!
!     --- BOUCLE SUR LE NOMBRE DE MISES EN FACTEUR ---
!     -----------------------------------------------------------------
    do 10 iocc = 1, nocc
!
        call irmfac(iocc, form, ifi, nive, versio,&
                    modele, noma, nomare, resure(iocc), lgmsh)
!
10  end do
    if (lcasts) then
        ibid=5
        write(ifc,'(A,I4)') ' ENREGISTREMENT DE TYPE',ibid
        if (nive .eq. 10) then
            ibid = 1
            write(ifc,'(A,I4)') 'LABEL AUTOMATIQUE :',ibid
        endif
    endif
!
!     -- IMPRESSION DES CARTES DE DONNEES DE CHAM_MATER,  ... :
    call w039ca(ifi, form)
!
!
9999  continue
    call jedema()
end subroutine
