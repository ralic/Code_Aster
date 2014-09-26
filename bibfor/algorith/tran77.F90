subroutine tran77(nomres, typres, nomin, basemo)
    implicit none
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! IN  : NOMRES : NOM UTILISATEUR POUR LA COMMANDE REST_SOUS_STRUC
! IN  : TYPRES : TYPE DE RESULTAT : 'DYNA_TRANS'
! IN  : NOMIN  : NOM UTILISATEUR DU CONCEPT TRAN_GENE AMONT
! IN  : BASEMO : NOM UTILISATEUR DU CONCEPT MODE_MECA AMONT
! ----------------------------------------------------------------------
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/gettco.h"
#include "asterfort/assert.h"
#include "asterfort/cnocre.h"
#include "asterfort/copmod.h"
#include "asterfort/dismoi.h"
#include "asterfort/extrac.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mdgeph.h"
#include "asterfort/rbph01.h"
#include "asterfort/rbph02.h"
#include "asterfort/refdcp.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/rstran.h"
#include "asterfort/titre.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcreb.h"
#include "asterfort/vtcrec.h"
#include "asterfort/vtdefs.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    character(len=24) :: valk(2)
! ----------------------------------------------------------------------
    integer :: i, j, itresu(8)
    integer :: foci, focf, fomi, fomf, fomo
    real(kind=8) :: r8b, epsi
    complex(kind=8) :: cbid
    character(len=8) :: k8b, blanc, basemo, crit, interp, basem2, mailla, nomres
    character(len=8) :: nomin, mode, nomma, matgen, nomgd
    character(len=14) :: numddl
    character(len=16) :: typres, type(8), typcha, typbas(8), concep
    character(len=19) :: kinst, knume, trange, typref(8), prof
    character(len=24) :: matric, chamno, crefe(2), nomcha, objve1, k24bid
    character(len=24) :: objve2, objve3, objve4
    aster_logical :: tousno, multap, leffor, prems
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: iarchi, ibid, ich, iadrif
    integer :: idec, idefm, idresu, inocmp
    integer :: inoecp, inuddl, inumno, iret, iretou, isk
    integer :: jc, jinst, jnume, linst, llcha
    integer :: lvale, n1, n2, n3, n4, nbcham, nbinsg
    integer :: nbinst, nbmode, nbnoeu, ncmp, neq, nfonct
    real(kind=8), pointer :: base(:) => null()
    real(kind=8), pointer :: vectgene(:) => null()
    character(len=24), pointer :: refn(:) => null()
    integer, pointer :: desc(:) => null()
    real(kind=8), pointer :: disc(:) => null()
    cbid = dcmplx(0.d0, 0.d0)
!-----------------------------------------------------------------------
    data blanc    /'        '/
!      DATA CHAMN2   /'&&TRAN77.CHAMN2'/
!      DATA NOMCMP   /'DX      ','DY      ','DZ      ',
!     &               'DRX     ','DRY     ','DRZ     '/
!     ------------------------------------------------------------------
    call jemarq()
    mode = basemo
    trange = nomin
    call gettco(nomin, concep)
    nomcha=' '
    numddl=' '
!
!     --- RECUPERATION DES ENTITES DU MAILLAGE SUR LESQUELLES ---
!     ---                PORTE LA RESTITUTION                 ---
    tousno = .true.
    prems = .true.
    call getvtx(' ', 'GROUP_NO', nbval=0, nbret=n1)
    call getvtx(' ', 'NOEUD', nbval=0, nbret=n2)
    call getvtx(' ', 'GROUP_MA', nbval=0, nbret=n3)
    call getvtx(' ', 'MAILLE', nbval=0, nbret=n4)
    if (n1+n2+n3+n4 .ne. 0) tousno = .false.
!
!     --- RECUPERATION DE LA BASE MODALE ---
!
    call jeveuo(trange//'.DESC', 'L', vi=desc)
    nbmode = desc(2)
!
!
    if (mode .eq. blanc) then
        call dismoi('REF_RIGI_PREM', trange, 'RESU_DYNA', repk=matgen)
        call dismoi('REF_INTD_PREM', trange, 'RESU_DYNA', repk=basemo)
        if (matgen(1:8) .ne. blanc) then
            call dismoi('REF_RIGI_PREM', basemo, 'RESU_DYNA', repk=matric)
            if (matric .ne. blanc) then
                call dismoi('NOM_NUME_DDL', matric, 'MATR_ASSE', repk=numddl)
                call dismoi('NOM_MAILLA', matric, 'MATR_ASSE', repk=mailla)
                if (tousno) call dismoi('NB_EQUA', matric, 'MATR_ASSE', repi=neq)
            else
                call dismoi('NUME_DDL', basemo, 'RESU_DYNA', repk=numddl)
                call dismoi('NOM_GD', numddl, 'NUME_DDL', repk=nomgd)
                call dismoi('NOM_MAILLA', numddl, 'NUME_DDL', repk=mailla)
                if (tousno) call dismoi('NB_EQUA', numddl, 'NUME_DDL', repi=neq)
            endif
        else
!  POUR LES CALCULS SANS MATRICE GENERALISEE (PROJ_MESU_MODAL)
            call dismoi('NUME_DDL', basemo, 'RESU_DYNA', repk=matric)
            if (matric(1:8) .eq. blanc) then
                call dismoi('REF_RIGI_PREM', basemo, 'RESU_DYNA', repk=matric)
                call dismoi('NOM_NUME_DDL', matric, 'MATR_ASSE', repk=numddl)
            else
                numddl = matric(1:8)
            endif
            call jeveuo(numddl//'.NUME.REFN', 'L', vk24=refn)
            matric = refn(1)
            mailla = matric(1:8)
            call dismoi('REF_RIGI_PREM', basemo, 'RESU_DYNA', repk=matric)
            if (tousno) call dismoi('NB_EQUA', numddl, 'NUME_DDL', repi=neq)
        endif
!
        basem2 = basemo
!
!
    else
!         --- BASE MODALE CALCULEE PAR SOUS-STRUCTURATION
!
        call rsexch('F', basemo, 'DEPL', 1, nomcha,&
                    iret)
        nomcha = nomcha(1:19)//'.REFE'
        call dismoi('NOM_GD', nomcha, 'CHAM_NO', repk=nomgd)
        call jeveuo(nomcha, 'L', llcha)
        k24bid=zk24(llcha)
        mailla = k24bid(1:8)
!
!------ON VERIFIE QUE L'UTILISATEUR A RENSEIGNE LE MEME SUPPORT DE
!------RESTITUTION DANS LE FICHIER DE COMMANDE
        call getvid(' ', 'SQUELETTE', scal=nomma, nbret=isk)
        if (isk .ne. 0) then
            if (nomma .ne. mailla) then
                valk (1) = nomma
                valk (2) = mailla
                call utmess('F', 'SOUSTRUC2_9', nk=2, valk=valk)
            endif
        endif
!
        crefe(1) = zk24(llcha)
        crefe(2) = zk24(llcha+1)
        if (tousno) then
            call dismoi('NB_EQUA', nomcha, 'CHAM_NO', repi=neq)
        endif
        basem2 = ' '
        call jeveuo(nomcha, 'L', iadrif)
        matric=zk24(iadrif+1)
        numddl=matric(1:14)
    endif
!
    multap = .false.
!
!     ---   RECUPERATION DES VECTEURS DEPLACEMENT, VITESSE ET   ---
!     --- ACCELERATION GENERALISES SUIVANT LES CHAMPS SOUHAITES ---
    nfonct = 0
    call rbph01(trange, nbcham, type, itresu, nfonct,&
                basem2, typref, typbas, tousno, multap)
!
!     --- RECUPERATION DES NUMEROS DES NOEUDS ET DES DDLS ASSOCIES ---
!     ---         DANS LE CAS D'UNE RESTITUTION PARTIELLE          ---
!
    if (.not. tousno) then
        objve1 = '&&TRAN77.NUME_NOEUD  '
        objve2 = '&&TRAN77.NOM_CMP     '
        objve3 = '&&TRAN77.NB_NEQ      '
        objve4 = '&&TRAN77.NUME_DDL    '
        call rbph02(mailla, numddl, nomcha, nomgd, neq,&
                    nbnoeu, objve1, ncmp, objve2, objve3,&
                    objve4)
        call jeveuo(objve1, 'L', inumno)
        call jeveuo(objve2, 'L', inocmp)
        call jeveuo(objve3, 'L', inoecp)
        call jeveuo(objve4, 'L', inuddl)
    endif
!
!     --- RECUPERATION DES INSTANTS ---
!
    call getvtx(' ', 'CRITERE', scal=crit, nbret=n1)
    call getvr8(' ', 'PRECISION', scal=epsi, nbret=n1)
    call getvtx(' ', 'INTERPOL', scal=interp, nbret=n1)
!
    knume = '&&TRAN77.NUM_RANG'
    kinst = '&&TRAN77.INSTANT'
    call rstran(interp, trange, ' ', 1, kinst,&
                knume, nbinst, iretou)
    if (iretou .ne. 0) then
        call utmess('F', 'UTILITAI4_24')
    endif
    call jeexin(kinst, iret)
    if (iret .gt. 0) then
        call jeveuo(kinst, 'L', jinst)
        call jeveuo(knume, 'L', jnume)
    endif
!
!     --- CREATION DE LA SD RESULTAT ---
    call rscrsd('G', nomres, typres, nbinst)
!
!     --- RESTITUTION SUR LA BASE REELLE ---
!
! VERIFICATION QU'IL Y UN DE CES MOTS CLEFS :
!  'LIST_INST', 'LIST_FREQ', 'INST' ou 'FREQ'
! A MOINS QUE L'ON NE SOIT DANS UN CAS DE DOUBLE RESTITUTION
! APRES UNE DOUBLE PROJECTION (PRESENCE DU MOT CLEF 'MODE_MECA')
    foci = 0
    focf = 0
    fomi = 0
    fomf = 0
    fomo = 0
    call getvid(' ', 'LIST_INST', scal=k8b, nbret=foci)
    call getvid(' ', 'LIST_FREQ', scal=k8b, nbret=focf)
    call getvr8(' ', 'INST', scal=r8b, nbret=fomi)
    call getvr8(' ', 'FREQ', scal=r8b, nbret=fomf)
    call getvid(' ', 'MODE_MECA', scal=k8b, nbret=fomo)
    if ((interp(1:3).ne.'NON') .and.&
        (&
        foci .eq. 0 .and. focf .eq. 0 .and. fomi .eq. 0 .and. fomf .eq. 0 .and. fomo .eq. 0&
        )) then
        call utmess('F', 'ALGORITH10_95')
    endif
!
    call jeveuo(trange//'.DISC', 'L', vr=disc)
    call jelira(trange//'.DISC', 'LONMAX', nbinsg)
    AS_ALLOCATE(vr=vectgene, size=nbmode)
    do ich = 1, nbcham
        leffor=.true.
        if (type(ich) .eq. 'DEPL' .or. type(ich) .eq. 'VITE' .or. type(ich) .eq. 'ACCE' .or.&
            type(ich) .eq. 'ACCE_ABSOLU') leffor=.false.
!
!            --- RECUPERATION DES DEFORMEES MODALES ---
!
        typcha = typbas(ich)
        call rsexch('F', basemo, typcha, 1, nomcha,&
                    iret)
        nomcha = nomcha(1:19)//'.VALE'
        call jeexin(nomcha, ibid)
        if (ibid .gt. 0) then
            nomcha(20:24)='.VALE'
        else
            nomcha(20:24)='.CELV'
        endif
        if (leffor) call jelira(nomcha, 'LONMAX', neq)
        AS_ALLOCATE(vr=base, size=nbmode*neq)
        if (tousno) then
            call copmod(basemo, champ=typcha, numer=numddl, bmodr=base, nequa=neq)
        else
            do j = 1, nbmode
                call rsexch('F', basemo, typcha, j, nomcha,&
                            iret)
                call jeexin(nomcha(1:19)//'.VALE', ibid)
                if (ibid .gt. 0) then
                    nomcha(20:24)='.VALE'
                else
                    nomcha(20:24)='.CELV'
                endif
                call jeveuo(nomcha, 'L', idefm)
                idec = 0
                do i = 1, nbnoeu
                    do jc = 1, ncmp
                        if (zi(inoecp-1+(i-1)*ncmp+jc) .eq. 1) then
                            idec = idec + 1
                            base(1+(j-1)*neq+idec-1) = zr( idefm+zi( inuddl+idec-1)-1 )
                        endif
                    end do
                end do
            end do
        endif
        iarchi = 0
        if (interp(1:3) .eq. 'NON') then
            call jeexin(trange//'.ORDR', iret)
            if (iret .ne. 0 .and. zi(jnume) .eq. 1) iarchi = -1
        endif
        idresu = itresu(ich)
        do i = 0, nbinst-1
            iarchi = iarchi + 1
            call rsexch(' ', nomres, type(ich), iarchi, chamno,&
                        iret)
            if (iret .eq. 0) then
                call utmess('A', 'ALGORITH2_64', sk=chamno)
            else if (iret .eq. 100) then
                if (tousno) then
                    if (mode .eq. blanc) then
                        if (leffor) then
                            call vtdefs(chamno, typref(ich), 'G', 'R')
                        else
                            call vtcreb(chamno, 'G', 'R',&
                                        nume_ddlz = numddl,&
                                        nb_equa_outz = neq)
                        endif
                    else
                        call vtcrec(chamno, nomcha, 'G', 'R', neq)
                    endif
                else
                    if (prems) then
                        prems=.false.
                        call cnocre(mailla, nomgd, nbnoeu, zi(inumno), ncmp,&
                                    zk8(inocmp), zi(inoecp), 'G', ' ', chamno)
                        call dismoi('PROF_CHNO', chamno, 'CHAM_NO', repk=prof)
                    else
                        call cnocre(mailla, nomgd, nbnoeu, zi( inumno), ncmp,&
                                    zk8(inocmp), zi(inoecp), 'G', prof, chamno)
                    endif
                endif
            else
                ASSERT(.false.)
            endif
            chamno(20:24) = '.VALE'
            call jeexin(chamno, ibid)
            if (ibid .gt. 0) then
                chamno(20:24) = '.VALE'
            else
                chamno(20:24) = '.CELV'
            endif
            call jeveuo(chamno, 'E', lvale)
!
            if (leffor .or. .not.tousno) call jelira(chamno, 'LONMAX', neq)
            if (interp(1:3) .ne. 'NON') then
                call extrac(interp, epsi, crit, nbinsg, disc,&
                            zr(jinst+i), zr(idresu), nbmode, vectgene, ibid)
                call mdgeph(neq, nbmode, base, vectgene, zr(lvale))
            else
                call mdgeph(neq, nbmode, base, zr(idresu+(zi( jnume+i)-1)*nbmode), zr(lvale))
            endif
!
            call rsnoch(nomres, type(ich), iarchi)
            call rsadpa(nomres, 'E', 1, 'INST', iarchi,&
                        0, sjv=linst, styp=k8b)
            zr(linst) = zr(jinst+i)
        end do
        AS_DEALLOCATE(vr=base)
    end do
!
!
!
    if (mode .eq. blanc) then
        call refdcp(basemo, nomres)
    endif
!
    call jedetr('&&TRAN77.NUME_NOEUD  ')
    call jedetr('&&TRAN77.NOM_CMP     ')
    call jedetr('&&TRAN77.NB_NEQ      ')
    call jedetr('&&TRAN77.NUME_DDL    ')
    call jedetr('&&TRAN77.NUM_RANG')
    call jedetr('&&TRAN77.INSTANT')
    AS_DEALLOCATE(vr=vectgene)
!
    call titre()
!
    call jedema()
end subroutine
