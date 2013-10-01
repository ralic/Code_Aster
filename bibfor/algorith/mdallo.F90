subroutine mdallo(nomres, basemo, masgen, riggen, amogen,&
                  nbmode, dt, nbsauv, nbchoc, noecho,&
                  intitu, nbrede, fonred, nbrevi, fonrev,&
                  jdepl, jvite, jacce, jptem, jordr,&
                  jdisc, jfcho, jdcho, jvcho, jadcho,&
                  jredc, jredd, jrevc, jrevv, method,&
                  nbsym, nomsym, typcal, sauve)
! aslint: disable=W1504
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jecreo.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jeveut.h"
#include "asterfort/r8inir.h"
#include "asterfort/refdaj.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
!
    integer :: nbrede, nbrevi, nbchoc
    character(len=*) :: basemo, masgen, riggen, amogen
    character(len=8) :: nomres, intitu(*)
    character(len=8) :: noecho(nbchoc, *), fonred(nbrede, *), fonrev(nbrevi, *)
    character(len=16) :: method
! ----------------------------------------------------------------------
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
!     ALLOCATION DES VECTEURS DE SORTIE POUR UN CALCUL TRANSITOIRE
!     SUR BASE GENERALISEE (SD_DYNA_GENE)
!     ------------------------------------------------------------------
! IN  : NOMRES : NOM DU RESULTAT
! IN  : BASEMO : NOM DU CONCEPT BASE MODALE
! IN  : MASGEN : NOM DU CONCEPT MASSE GENERALISEE
! IN  : RIGGEN : NOM DU CONCEPT RAIDEUR GENERALISEE
! IN  : AMOGEN : NOM DU CONCEPT AMORTISSEMENT GENERALISE
! IN  : NBMODE : NOMBRE DE MODES
! IN  : DT     : PAS DE TEMPS
! IN  : NBPAS  : NOMBRE DE PAS CALCULE (INITIAL COMPRIS)
! IN  : NBCHOC : NOMBRE DE NOEUDS DE CHOC
! IN  : NOECHO : TABLEAU DES NOMS DES NOEUDS DE CHOC
! IN  : INTITU : TABLEAU DES NOMS DES LIAISONS
! IN  : NBREDE : NOMBRE DE RELATION EFFORT DEPLACEMENT (RED)
! IN  : FONRED : TABLEAU DES FONCTIONS DE RED
! IN  : NBREVI : NOMBRE DE RELATION EFFORT VITESSE (REV)
! IN  : METHOD : ALGORITHME UTILISE (DEVOGE, EULER, ...)
!                DANS LE CAS ITMI, UN OBJET EST DIFFERENT
! IN  : TYPCAL : VAUT 'HARM' OU 'TRAN'
! IN  : SAUVE :  VAUT 'GLOB' OU 'VOLA'
! ----------------------------------------------------------------------
    integer :: nbsauv, nbstoc, j1refe, nbsym, inom
    character(len=8) :: numgen, blanc
    character(len=5) :: attrib
    character(len=4) :: typcal, nomsym(*), sauve
    character(len=3) :: typsau
    character(len=12) :: bl11pt
    character(len=24) :: matric(3)
!
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    logical :: entvid
    integer :: i, ic, iret, jacce, jadcho, jdcho, jdepl, jchmp
    integer :: jdesc, jfcho, jdisc, jinti, jncho, jordr, jptem
    integer :: jredc, jredd, jredn, jrevc, jrevv, jrevn
    integer :: jsst, jvcho, jvint
    integer :: jvite, nbmode, nbsto1
    real(kind=8) :: dt
!-----------------------------------------------------------------------
    call jemarq()
    ASSERT((sauve(1:4).eq.'GLOB'.or.sauve(1:4).eq.'VOLA'))
    if (sauve(1:4) .eq. 'GLOB') typsau='G V'
    if (sauve(1:4) .eq. 'VOLA') typsau='V V'
    nbstoc = nbmode * nbsauv
!
    jdepl=1
    jvite=1
    jacce=1
    jptem=1
    jordr=1
    jdisc=1
    jdesc=1
    jchmp=1
    jfcho = 1
    jdcho = 1
    jvcho = 1
    jadcho= 1
    jredc = 1
    jredd = 1
    jrevc = 1
    jrevv = 1
    blanc = '        '
    bl11pt = '           .'
!
    call jeexin(nomres//'           .REFD', iret)
    entvid = .false.
    if ((riggen .eq. ' ') .and. (masgen .eq. ' ') .and. (amogen .eq. ' ')) entvid = .true.
!
    if (iret .eq. 0) then
        if (entvid) then
            if (basemo .ne. blanc) then
                matric(1) = basemo
                call refdaj('F', nomres, nbsauv, ' ', 'MESURE',&
                            matric, iret)
            else
                call refdaj(' ', nomres, nbsauv, ' ', 'INIT',&
                            ' ', iret)
            endif
        else
!           On recupere la numerotation generalisee
            call jeexin(riggen(1:8)//'           .REFA', iret)
            if (iret .ne. 0) then
                call jeveuo(riggen(1:8)//'           .REFA', 'L', j1refe)
                numgen = zk24(j1refe+1)(1:8)
            else
                numgen = blanc
            endif
            matric(1) = riggen(1:8)
            matric(2) = masgen(1:8)
            matric(3) = amogen(1:8)
            call refdaj('F', nomres, nbsauv, numgen(1:8), 'DYNAMIQUE',&
                        matric, iret)
        endif
    endif
!
    call jeexin(nomres//'           .DESC', iret)
    if (iret .eq. 0) then
        call wkvect(nomres//'           .DESC', typsau//' I', 5, jdesc)
!
        zi(jdesc) = 1
!
        if (typcal .eq. 'HARM') then
            zi(jdesc) = 4
!          -- DANS LE CAS 'HARM' ON REMPLIT LA VALEUR A 4
!
!          -- BLINDAGE : VERIFICATION DE NBSYM ET NOMSYM
            if ((nbsym.le.0) .or. (nbsym.ge.4)) then
                call utmess('F', 'ALGORITH17_29')
            endif
            do inom = 1, nbsym
                if ((nomsym(inom)(1:4).ne.'DEPL') .and. (nomsym(inom)( 1:4).ne.'VITE')&
                    .and. (nomsym(inom)(1:4).ne.'ACCE')) then
                    call utmess('F', 'ALGORITH17_29')
                endif
            end do
        else if (typcal.eq.'TRAN') then
!         -- INITIALISATION DES CHAMPS A ALLOUER DANS LE CAS TRANS.
            nbsym = 3
            nomsym(1) = 'DEPL'
            nomsym(2) = 'VITE'
            nomsym(3) = 'ACCE'
            if (nbchoc .ne. 0) then
                zi(jdesc) = 2
            endif
!         -DANS LE CAS ITMI ET ADAPT (METHODES A PAS VARIABLE),
!          ON MET LA VALEUR 3 QUI SERVIRA DE TEST
!           A LA COMMANDE POST_DYNA_MODA_T
            if (method .eq. 'ITMI' .or. method(1:5) .eq. 'ADAPT' .or. method( 1:5) .eq.&
                'RUNGE') then
                zi(jdesc) = 3
            endif
!         DANS LE CAS TRANSITOIRE, ON REMPLIT TOUJOURS LES TROIS CHAMPS
        endif
!        ---
        zi(jdesc+1) = nbmode
        zi(jdesc+2) = nbchoc
        zi(jdesc+3) = nbrede
        zi(jdesc+4) = nbrevi
    endif
!
    if (typcal .eq. 'TRAN') then
        attrib = typsau//' R'
        nbsym = 3
        nomsym(1) = 'DEPL'
        nomsym(2) = 'VITE'
        nomsym(3) = 'ACCE'
    else
        attrib = typsau//' C'
    endif
!
    if (nbsauv .ne. 0) then
!       BOUCLE SUR LES CHAMPS A SAUVEGARDER (DEPL/VITE/ACCE)
        do inom = 1, nbsym
!
            call jecreo(nomres//bl11pt//nomsym(inom), attrib)
            call jeecra(nomres//bl11pt//nomsym(inom), 'LONMAX', nbstoc)
            call jeecra(nomres//bl11pt//nomsym(inom), 'LONUTI', nbstoc)
            call jeveut(nomres//bl11pt//nomsym(inom), 'E', jchmp)
!
!         INITIALISATION DES CHAMPS A ZERO
!
            if (typcal .eq. 'TRAN') then
                do i = 0, nbstoc-1
                    zr(jchmp+i) = 0.d0
                end do
            else
                do i = 0, nbstoc-1
                    zc(jchmp+i) = dcmplx(0.d0,0.d0)
                end do
            endif
            if (nomsym(inom) .eq. 'DEPL') jdepl=jchmp
            if (nomsym(inom) .eq. 'VITE') jvite=jchmp
            if (nomsym(inom) .eq. 'ACCE') jacce=jchmp
        end do
!
!       OBJETS COMMUNS
        call jecreo(nomres//'           .ORDR', typsau//' I')
        call jeecra(nomres//'           .ORDR', 'LONMAX', nbsauv)
        call jeecra(nomres//'           .ORDR', 'LONUTI', nbsauv)
        call jeveut(nomres//'           .ORDR', 'E', jordr)
        call jecreo(nomres//'           .DISC', typsau//' R')
        call jeecra(nomres//'           .DISC', 'LONMAX', nbsauv)
        call jeecra(nomres//'           .DISC', 'LONUTI', nbsauv)
        call jeveut(nomres//'           .DISC', 'E', jdisc)
!
        if (typcal .eq. 'TRAN') then
            call jecreo(nomres//'           .PTEM', typsau//' R')
            call jeecra(nomres//'           .PTEM', 'LONMAX', nbsauv)
            call jeecra(nomres//'           .PTEM', 'LONUTI', nbsauv)
            call jeveut(nomres//'           .PTEM', 'E', jptem)
            zr(jptem) = dt
        endif
    endif
!
!     --- CREATION DES VECTEURS DE STOCKAGE DES FORCES DE CHOC ---
    if (nbchoc .ne. 0) then
        nbstoc = 3 * nbchoc * nbsauv
        nbsto1 = nbchoc * nbsauv
        call jeexin(nomres//'           .NCHO', iret)
        if (iret .eq. 0) call wkvect(nomres//'           .NCHO', typsau//' K8', 2*nbchoc, jncho)
        call jeexin(nomres//'           .SST', iret)
        if (iret .eq. 0) call wkvect(nomres//'           .SST', typsau//' K8', 2*nbchoc, jsst)
        if (nbsauv .ne. 0) then
            call jecreo(nomres//'           .FCHO', typsau//' R')
            call jeecra(nomres//'           .FCHO', 'LONMAX', nbstoc)
            call jeecra(nomres//'           .FCHO', 'LONUTI', nbstoc)
            call jeveut(nomres//'           .FCHO', 'E', jfcho)
            call jecreo(nomres//'           .DLOC', typsau//' R')
            call jeecra(nomres//'           .DLOC', 'LONMAX', 2*nbstoc)
            call jeecra(nomres//'           .DLOC', 'LONUTI', 2*nbstoc)
            call jeveut(nomres//'           .DLOC', 'E', jdcho)
            call jecreo(nomres//'           .VCHO', typsau//' R')
            call jeecra(nomres//'           .VCHO', 'LONMAX', nbstoc)
            call jeecra(nomres//'           .VCHO', 'LONUTI', nbstoc)
            call jeveut(nomres//'           .VCHO', 'E', jvcho)
            call jecreo(nomres//'           .ICHO', typsau//' I')
            call jeecra(nomres//'           .ICHO', 'LONMAX', nbsto1)
            call jeecra(nomres//'           .ICHO', 'LONUTI', nbsto1)
            call jeveut(nomres//'           .ICHO', 'E', jadcho)
!          --- OBJET POUR LE FLAMBEMENT : VARIABLE INTERNE ---
            call jecreo(nomres//'           .VINT', typsau//' R')
            call jeecra(nomres//'           .VINT', 'LONMAX', nbsto1)
            call jeecra(nomres//'           .VINT', 'LONUTI', nbsto1)
!              INITIALISATION
            call jeveuo(nomres//'           .VINT', 'E', jvint)
            call r8inir(nbsto1, 0.d0, zr(jvint), 1)
        endif
        call jeexin(nomres//'           .INTI', iret)
        if (iret .eq. 0) then
            call wkvect(nomres//'           .INTI', typsau//' K8', nbchoc, jinti)
            do ic = 1, nbchoc
                zk8(jinti+ic-1) = intitu(ic)
                zk8(jncho+ic-1) = noecho(ic,1)
                zk8(jncho+nbchoc+ic-1) = noecho(ic,5)
                zk8(jsst+ic-1) = noecho(ic,2)
                zk8(jsst+nbchoc+ic-1) = noecho(ic,6)
            end do
        endif
    endif
!
!     --- CREATION DES VECTEURS DE STOCKAGE DES RELA_EFFO_DEPL ---
    if (nbrede .ne. 0) then
        nbstoc = nbrede * nbsauv
        if (nbsauv .ne. 0) then
            call jecreo(nomres//'           .REDC', typsau//' I')
            call jeecra(nomres//'           .REDC', 'LONMAX', nbstoc)
            call jeecra(nomres//'           .REDC', 'LONUTI', nbstoc)
            call jeveut(nomres//'           .REDC', 'E', jredc)
            call jecreo(nomres//'           .REDD', typsau//' R')
            call jeecra(nomres//'           .REDD', 'LONMAX', nbstoc)
            call jeecra(nomres//'           .REDD', 'LONUTI', nbstoc)
            call jeveut(nomres//'           .REDD', 'E', jredd)
        endif
        call jeexin(nomres//'           .REDN', iret)
        if (iret .eq. 0) then
            call wkvect(nomres//'           .REDN', typsau//' K24', nbrede, jredn)
            do i = 1, nbrede
                zk24(jredn+i-1) = fonred(i,1)//fonred(i,2)//fonred(i, 3)
            end do
        endif
    endif
!
!     --- CREATION DES VECTEURS DE STOCKAGE DES RELA_EFFO_VITE ---
    if (nbrevi .ne. 0) then
        nbstoc = nbrevi * nbsauv
        if (nbsauv .ne. 0) then
            call jecreo(nomres//'           .REVC', typsau//' I')
            call jeecra(nomres//'           .REVC', 'LONMAX', nbstoc)
            call jeecra(nomres//'           .REVC', 'LONUTI', nbstoc)
            call jeveut(nomres//'           .REVC', 'E', jrevc)
            call jecreo(nomres//'           .REVV', typsau//' R')
            call jeecra(nomres//'           .REVV', 'LONMAX', nbstoc)
            call jeecra(nomres//'           .REVV', 'LONUTI', nbstoc)
            call jeveut(nomres//'           .REVV', 'E', jrevv)
        endif
        call jeexin(nomres//'           .REVN', iret)
        if (iret .eq. 0) then
            call wkvect(nomres//'           .REVN', typsau//' K24', nbrevi, jrevn)
            do i = 1, nbrevi
                zk24(jrevn+i-1) = fonrev(i,1)//fonrev(i,2)//fonrev(i, 3)
            end do
        endif
    endif
!
    call jedema()
end subroutine
